module Bach.Fugue
    ( runFugue
    , validateBatch
    , selectBatch
    ) where

import Bach.Batching (buildBatches)
import Bach.Conflicts (findConflicts, nPairs, partitionBase)
import Bach.Forge (HasForgeHandle (..), fetchPR)
import Bach.Git (detectRepoContext, gitCommitTree, gitFetch, gitMergeTree)
import Bach.Prelude
import Bach.Types
import qualified Data.Text as T
import RIO.Directory (getCurrentDirectory)
import RIO.List (sortOn)
import qualified RIO.Map as Map
import qualified RIO.Set as Set

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
        $ "Detected repo: "
        <> display (repoOwner ctx)
        <> "/"
        <> display (repoName ctx)
    logInfo $ "Base branch: " <> display base

    -- Fetch PR metadata from forge
    prs <- forM (fugueTargets opts) $ \prid -> do
        pr <- fetchPR prid
        logInfo $ "  #" <> display pr.prNumber <> " " <> display pr.prTitle
        pure pr

    -- Resolve must-include PRs
    mustIncludeSet <- resolveMustInclude prs (fugueMustInclude opts)
    unless (Set.null mustIncludeSet)
        $ logInfo
        $ "Must-include: "
        <> displayShow (Set.toList mustIncludeSet)

    -- Fetch git refs
    unless (fugueNoFetch opts) $ do
        logInfo "Fetching refs..."
        gitFetch dir (base : map (.prHeadRef) prs)

    -- Phase 1a: Partition by base conflicts
    logInfo "Checking for base conflicts..."
    (baseConflicts, validPRs) <- partitionBase dir base prs
    unless (null baseConflicts)
        $ logWarn
        $ displayShow (length baseConflicts)
        <> " PR(s) conflict with base and were excluded"

    -- Check must-include PRs aren't base-conflicting
    let
        mustIncludeBaseConflicts =
            filter (\pr -> Set.member pr.prNumber mustIncludeSet) baseConflicts
    unless (null mustIncludeBaseConflicts)
        $ throwIO
        $ MustIncludeError
        $ "Must-include PR(s) conflict with base: "
        <> T.intercalate
            ", "
            (map (\pr -> "#" <> tshow pr.prNumber) mustIncludeBaseConflicts)

    -- Phase 1b: Pairwise conflict detection
    logInfo
        $ "Testing "
        <> displayShow (length validPRs)
        <> " PRs for pairwise conflicts ("
        <> displayShow (nPairs (length validPRs))
        <> " pairs)..."
    conflictPairs <- findConflicts dir base validPRs

    -- Check must-include PRs don't conflict with each other
    let
        mustIncludeConflicts =
            filter
                ( \cp ->
                    Set.member cp.cpLeft mustIncludeSet
                        && Set.member cp.cpRight mustIncludeSet
                )
                conflictPairs
    unless (null mustIncludeConflicts)
        $ throwIO
        $ MustIncludeError
        $ "Must-include PRs conflict with each other: "
        <> T.intercalate
            ", "
            ( map
                (\cp -> "#" <> tshow cp.cpLeft <> " vs #" <> tshow cp.cpRight)
                mustIncludeConflicts
            )

    -- Graph coloring
    let
        conflictSet =
            Set.fromList $ map (\cp -> (cp.cpLeft, cp.cpRight)) conflictPairs
        batches = buildBatches validPRs conflictSet

    -- Select candidate batch: largest batch containing all must-include PRs
    (candidateBatch, deferred) <-
        selectBatch mustIncludeSet batches

    logInfo
        $ displayShow (length candidateBatch)
        <> " PR(s) in candidate batch, "
        <> displayShow (length deferred)
        <> " deferred by pairwise conflicts"

    -- Phase 2: Validate candidate batch by sequential merge-tree
    -- Must-include PRs go first so they're never evicted by others
    logInfo "Validating batch..."
    let
        mustFirst =
            filter (\pr -> Set.member pr.prNumber mustIncludeSet) candidateBatch
        others =
            filter (\pr -> Set.notMember pr.prNumber mustIncludeSet) candidateBatch
        orderedBatch = mustFirst <> others
    (ready, evicted) <- validateBatch dir base orderedBatch

    -- Check must-include PRs weren't evicted (higher-order conflict among them)
    let
        mustIncludeEvicted =
            filter (\pr -> Set.member pr.prNumber mustIncludeSet) evicted
    unless (null mustIncludeEvicted)
        $ throwIO
        $ MustIncludeError
        $ "Must-include PR(s) have higher-order conflicts: "
        <> T.intercalate
            ", "
            (map (\pr -> "#" <> tshow pr.prNumber) mustIncludeEvicted)

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

-- | Resolve must-include identifiers against the fetched PR list.
resolveMustInclude
    :: (MonadIO m) => [PullRequest] -> [PRIdentifier] -> m (Set.Set Int)
resolveMustInclude _ [] = pure Set.empty
resolveMustInclude prs ids = Set.fromList <$> mapM resolve ids
  where
    resolve (PRById n) =
        case filter (\pr -> pr.prNumber == n) prs of
            (_ : _) -> pure n
            [] ->
                throwIO
                    $ MustIncludeError
                    $ "Must-include PR #"
                    <> tshow n
                    <> " not found in targets"
    resolve (PRByBranch b) =
        case filter (\pr -> pr.prHeadRef == b) prs of
            [pr] -> pure pr.prNumber
            [] ->
                throwIO
                    $ MustIncludeError
                    $ "Must-include branch '"
                    <> b
                    <> "' not found in targets"
            _ ->
                throwIO
                    $ MustIncludeError
                    $ "Multiple PRs match must-include branch '"
                    <> b
                    <> "'"

-- | Select the best batch: the largest one containing all must-include PRs.
-- When must-include is empty, just picks the largest batch.
-- Returns (selected batch, all other PRs).
selectBatch
    :: (MonadIO m)
    => Set.Set Int
    -> Map.Map Int [PullRequest]
    -> m ([PullRequest], [PullRequest])
selectBatch mustInclude batches
    | Map.null batches = pure ([], [])
    | otherwise = do
        let
            batchList = Map.toList batches
            eligible =
                filter (containsMustInclude . snd) batchList
            sorted =
                sortOn (negate . length . snd) eligible
        case sorted of
            [] ->
                throwIO
                    $ MustIncludeError "No batch contains all must-include PRs"
            ((chosenKey, chosen) : _) ->
                let
                    deferred =
                        concatMap snd
                            $ filter (\(k, _) -> k /= chosenKey) batchList
                 in
                    pure (chosen, deferred)
  where
    containsMustInclude prs =
        let
            prNums = Set.fromList $ map (.prNumber) prs
         in
            Set.isSubsetOf mustInclude prNums

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
                logInfo $ "    #" <> display pr.prNumber <> " evicted (higher-order conflict)"
                go rest accumulated clean (pr : evicted)
