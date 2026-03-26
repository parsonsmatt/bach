module Bach.Fugue
    ( runFugue
    , validateBatch
    , MustIncludeBaseConflict (..)
    , MustIncludePairwiseConflict (..)
    , MustIncludeHigherOrderConflict (..)
    ) where

import Bach.Batching (buildBatches, selectBatch)
import Bach.Conflicts (findConflicts, nPairs, partitionBase)
import Bach.Forge (HasForgeHandle (..), fetchPR)
import Bach.Git (detectRepoContext, gitCommitTree, gitFetch, gitMergeTree)
import Bach.PRMap (PRMap)
import qualified Bach.PRMap as PRMap
import Bach.Prelude
import Bach.Types
import Data.List (intercalate)
import Data.List.NonEmpty (nonEmpty)
import Data.These (These (..))
import RIO.Directory (getCurrentDirectory)
import qualified RIO.Set as Set

data MustIncludeBaseConflict = MustIncludeBaseConflict !(NonEmpty PullRequest)
    deriving stock (Show, Eq)

instance Exception MustIncludeBaseConflict where
    displayException (MustIncludeBaseConflict prs) =
        "Must-include PR(s) conflict with base: " <> showPRNums prs

data MustIncludePairwiseConflict
    = MustIncludePairwiseConflict !(NonEmpty ConflictPair)
    deriving stock (Show, Eq)

instance Exception MustIncludePairwiseConflict where
    displayException (MustIncludePairwiseConflict cps) =
        "Must-include PRs conflict with each other: "
            <> intercalate
                ", "
                ( map
                    (\cp -> mconcat ["#", show cp.cpLeft, " vs #", show cp.cpRight])
                    (toList cps)
                )

data MustIncludeHigherOrderConflict
    = MustIncludeHigherOrderConflict !(NonEmpty PullRequest)
    deriving stock (Show, Eq)

instance Exception MustIncludeHigherOrderConflict where
    displayException (MustIncludeHigherOrderConflict prs) =
        "Must-include PR(s) have higher-order conflicts: " <> showPRNums prs

showPRNums :: NonEmpty PullRequest -> String
showPRNums = intercalate ", " . map (\pr -> "#" <> show pr.prNumber) . toList

-- | Run the fugue algorithm: pairwise conflict detection + graph coloring
-- + sequential validation. Returns the largest conflict-free batch (ready)
-- and everything else (deferred).
runFugue
    :: (HasForgeHandle env, HasLogFunc env)
    => FugueOptions -> RIO env FugueResults
runFugue opts = do
    cwd <- liftIO getCurrentDirectory
    ctx <- detectRepoContext cwd
    let
        base = fromMaybe (repoDefaultBase ctx) (fugueBase opts)
        dir = repoLocalPath ctx

    logInfo
        $ mconcat
            [ "Detected repo: "
            , display (repoOwner ctx)
            , "/"
            , display (repoName ctx)
            ]
    logInfo $ "Base branch: " <> display base

    -- Fetch all PR metadata, deduplicating by PR number
    logInfo "Fetching PR metadata..."
    (prMap, mustIncludeSet) <- fetchAllPRs opts

    -- PRMap built from NonEmpty targets is always non-empty
    prs <- case nonEmpty (PRMap.elems prMap) of
        Just ne -> pure ne
        Nothing -> error "runFugue: impossible — targets is NonEmpty"

    unless (Set.null mustIncludeSet)
        $ logInfo
        $ "Must-include: "
        <> displayShow (Set.toList mustIncludeSet)

    -- Fetch git refs
    unless (fugueNoFetch opts) $ do
        logInfo "Fetching refs..."
        gitFetch dir (base : map (.prHeadRef) (toList prs))

    -- Phase 1a: Partition by base conflicts
    logInfo "Checking for base conflicts..."
    partitioned <- partitionBase dir base prs

    let
        baseConflicts = case partitioned of
            This cs -> toList cs
            These cs _ -> toList cs
            That _ -> []

    unless (null baseConflicts)
        $ logWarn
        $ displayShow (length baseConflicts)
        <> " PR(s) conflict with base and were excluded"

    -- Check must-include PRs aren't base-conflicting
    let
        isMustInclude pr = Set.member pr.prNumber mustIncludeSet
    forM_ (nonEmpty $ filter isMustInclude baseConflicts)
        $ throwIO
        . MustIncludeBaseConflict

    case partitioned of
        This _ -> do
            logInfo "No valid PRs to batch"
            pure
                FugueResults
                    { frBaseConflicts = baseConflicts
                    , frConflictPairs = []
                    , frReady = []
                    , frDeferred = []
                    }
        That validPRs -> runBatching mustIncludeSet dir base baseConflicts validPRs
        These _ validPRs -> runBatching mustIncludeSet dir base baseConflicts validPRs

-- | Fetch target and must-include PRs, returning a deduplicated map
-- and the set of must-include PR numbers.
fetchAllPRs
    :: (HasForgeHandle env, HasLogFunc env)
    => FugueOptions
    -> RIO env (PRMap, Set.Set Int)
fetchAllPRs opts = do
    prMap <-
        foldM
            (\m prid -> fst <$> fetchPRInto Nothing m prid)
            PRMap.empty
            (toList (fugueTargets opts))
    foldM
        ( \(m, s) prid -> do
            (m', prNum) <- fetchPRInto (Just "(must-include)") m prid
            pure (m', Set.insert prNum s)
        )
        (prMap, Set.empty)
        (fugueMustInclude opts)

-- | Fetch a PR and insert into the map. Logs with an optional suffix
-- if the PR is new. Returns the updated map and the PR number.
fetchPRInto
    :: (HasForgeHandle env, HasLogFunc env)
    => Maybe Utf8Builder
    -> PRMap
    -> PRIdentifier
    -> RIO env (PRMap, Int)
fetchPRInto mSuffix prMap prid = do
    pr <- fetchPR prid
    let
        (isNew, prMap') = PRMap.insert pr prMap
    when isNew
        $ logInfo
        $ mconcat
            [ "  #"
            , display pr.prNumber
            , " "
            , display pr.prTitle
            , foldMap (" " <>) mSuffix
            ]
    pure (prMap', pr.prNumber)

-- | Run pairwise conflict detection, graph coloring, and sequential
-- validation on the non-empty set of valid PRs.
runBatching
    :: (HasLogFunc env)
    => Set.Set Int
    -> FilePath
    -> Text
    -> [PullRequest]
    -> NonEmpty PullRequest
    -> RIO env FugueResults
runBatching mustIncludeSet dir base baseConflicts validPRs = do
    let
        isMustInclude pr = Set.member pr.prNumber mustIncludeSet

    -- Phase 1b: Pairwise conflict detection
    logInfo
        $ mconcat
            [ "Testing "
            , displayShow (length validPRs)
            , " PRs for pairwise conflicts ("
            , displayShow (nPairs (length validPRs))
            , " pairs)..."
            ]
    mConflictPairs <- findConflicts dir base validPRs
    let
        conflictPairs = maybe [] toList mConflictPairs

    -- Check must-include PRs don't conflict with each other
    let
        isMustIncludeConflict cp =
            Set.member cp.cpLeft mustIncludeSet
                && Set.member cp.cpRight mustIncludeSet
    forM_ (nonEmpty $ filter isMustIncludeConflict conflictPairs)
        $ throwIO
        . MustIncludePairwiseConflict

    -- Graph coloring
    let
        conflictSet =
            Set.fromList $ map (\cp -> (cp.cpLeft, cp.cpRight)) conflictPairs
        batches = buildBatches validPRs conflictSet

    -- Select candidate batch: largest containing all must-include PRs
    (candidateBatch, deferred) <-
        either throwIO pure $ selectBatch mustIncludeSet batches

    let
        candidateList = toList candidateBatch

    logInfo
        $ mconcat
            [ displayShow (length candidateBatch)
            , " PR(s) in candidate batch, "
            , displayShow (length deferred)
            , " deferred by pairwise conflicts"
            ]

    -- Phase 2: Validate candidate batch by sequential merge-tree
    -- Must-include PRs go first so they're never evicted by others
    logInfo "Validating batch..."
    let
        mustFirst = filter isMustInclude candidateList
        others = filter (not . isMustInclude) candidateList
        orderedBatch = mustFirst <> others
    (ready, evicted) <- validateBatch dir base orderedBatch

    -- Check must-include PRs weren't evicted
    forM_ (nonEmpty $ filter isMustInclude evicted)
        $ throwIO
        . MustIncludeHigherOrderConflict

    unless (null evicted)
        $ logWarn
        $ displayShow (length evicted)
        <> " PR(s) evicted during validation (higher-order conflicts)"

    logInfo
        $ displayShow (length ready)
        <> " PR(s) ready to merge"

    pure
        FugueResults
            { frBaseConflicts = baseConflicts
            , frConflictPairs = conflictPairs
            , frReady = ready
            , frDeferred = deferred <> evicted
            }

-- | Validate a batch by sequentially merging each PR into an accumulated
-- tree via merge-tree. Returns (clean PRs, evicted PRs).
validateBatch
    :: (HasLogFunc env)
    => FilePath
    -> Text
    -> [PullRequest]
    -> RIO env ([PullRequest], [PullRequest])
validateBatch dir base prs = go prs ("origin/" <> base) [] []
  where
    go [] _ clean evicted = pure (reverse clean, reverse evicted)
    go (pr : rest) accumulated clean evicted = do
        result <- gitMergeTree dir accumulated ("origin/" <> pr.prHeadRef)
        case result of
            MergeOk tree -> do
                mCommit <- gitCommitTree dir tree accumulated ("origin/" <> pr.prHeadRef)
                case mCommit of
                    Just commit -> go rest commit (pr : clean) evicted
                    Nothing -> go rest accumulated clean (pr : evicted)
            MergeConflict _ -> do
                logInfo
                    $ mconcat ["    #", display pr.prNumber, " evicted (higher-order conflict)"]
                go rest accumulated clean (pr : evicted)
