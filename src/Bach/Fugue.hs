module Bach.Fugue
    ( runFugue
    , validateBatch
    ) where

import Bach.Batching (buildBatches)
import Bach.Conflicts (findConflicts, nPairs, partitionBase)
import Bach.Forge (HasForgeHandle (..), fetchPR)
import Bach.Git (detectRepoContext, gitCommitTree, gitFetch, gitMergeTree)
import Bach.Prelude
import Bach.Types
import RIO.Directory (getCurrentDirectory)
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

    -- Phase 1b: Pairwise conflict detection
    logInfo
        $ "Testing "
        <> displayShow (length validPRs)
        <> " PRs for pairwise conflicts ("
        <> displayShow (nPairs (length validPRs))
        <> " pairs)..."
    conflictPairs <- findConflicts dir base validPRs

    -- Graph coloring to find batch 1 (largest conflict-free set)
    let
        conflictSet = Set.fromList $ map (\cp -> (cp.cpLeft, cp.cpRight)) conflictPairs
        batches = buildBatches validPRs conflictSet
        batch1 = fromMaybe [] $ Map.lookup 1 batches
        deferred = concatMap snd $ filter (\(k, _) -> k /= 1) $ Map.toList batches

    logInfo
        $ displayShow (length batch1)
        <> " PR(s) in candidate batch, "
        <> displayShow (length deferred)
        <> " deferred by pairwise conflicts"

    -- Phase 2: Validate batch 1 by sequential merge-tree
    logInfo "Validating batch..."
    (ready, evicted) <- validateBatch dir base batch1

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
                logInfo $ "    #" <> display pr.prNumber <> " evicted (higher-order conflict)"
                go rest accumulated clean (pr : evicted)
