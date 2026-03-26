module Bach.Conflicts
    ( partitionBase
    , findConflicts
    , nPairs
    ) where

import Bach.Git (gitCommitTree, gitMergeTree)
import Bach.Prelude
import Bach.Types
import Data.List (tails, uncons)

-- | Partition PRs into (conflicts with base, clean).
partitionBase
    :: (HasLogFunc env, Foldable f)
    => FilePath -> Text -> f PullRequest -> RIO env ([PullRequest], [PullRequest])
partitionBase dir base prs = do
    results <- forM (toList prs) $ \pr -> do
        result <- gitMergeTree dir ("origin/" <> base) ("origin/" <> pr.prHeadRef)
        case result of
            MergeConflict _ -> do
                logWarn $ "  #" <> display pr.prNumber <> " conflicts with base"
                pure $ Left pr
            MergeOk _ -> pure $ Right pr
    pure $ partitionEithers results

-- | For each pair of PRs, test if they conflict when both merged into the base.
-- Uses @git merge-tree@ (in-memory) and temporary commits to avoid touching
-- the working directory.
findConflicts
    :: (HasLogFunc env)
    => FilePath -> Text -> [PullRequest] -> RIO env [ConflictPair]
findConflicts dir base prs = do
    let
        groups = mapMaybe uncons (tails prs)
        total = nPairs (length prs)
    indexRef <- newIORef (0 :: Int)
    fmap (catMaybes . concat)
        $ forM groups
        $ \(leftPR, rightPRs) -> do
            mergeResult <-
                gitMergeTree dir ("origin/" <> base) ("origin/" <> leftPR.prHeadRef)
            case mergeResult of
                MergeConflict _ -> pure []
                MergeOk tree -> do
                    mTempCommit <-
                        gitCommitTree dir tree ("origin/" <> base) ("origin/" <> leftPR.prHeadRef)
                    case mTempCommit of
                        Nothing -> pure []
                        Just tempCommit ->
                            forM rightPRs $ \rightPR -> do
                                idx <- atomicModifyIORef' indexRef $ \i -> (i + 1, i + 1)
                                result <- gitMergeTree dir tempCommit ("origin/" <> rightPR.prHeadRef)
                                case result of
                                    MergeConflict files -> do
                                        logInfo
                                            $ "  "
                                            <> displayShow idx
                                            <> "/"
                                            <> displayShow total
                                            <> " — #"
                                            <> display leftPR.prNumber
                                            <> " conflicts with #"
                                            <> display rightPR.prNumber
                                            <> " in "
                                            <> displayShow (length files)
                                            <> " file(s)"
                                        pure $ Just $ ConflictPair leftPR.prNumber rightPR.prNumber files
                                    MergeOk _ -> do
                                        when (idx `mod` 10 == 0)
                                            $ logInfo
                                            $ "  "
                                            <> displayShow idx
                                            <> "/"
                                            <> displayShow total
                                            <> " pairs tested"
                                        pure Nothing

nPairs :: Int -> Int
nPairs count = (count * (count - 1)) `div` 2
