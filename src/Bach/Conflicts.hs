module Bach.Conflicts
    ( partitionBase
    , findConflicts
    , nPairs
    ) where

import Bach.Git (gitCommitTree, gitMergeTree)
import Bach.Prelude
import Bach.These (partitionNE)
import Bach.Types
import Data.List (tails, uncons)
import Data.List.NonEmpty (nonEmpty)
import Data.These (These)

-- | Partition PRs into (conflicts with base, clean). Since the input is
-- non-empty, at least one side of the result is guaranteed non-empty.
partitionBase
    :: (HasLogFunc env)
    => FilePath
    -> Text
    -> NonEmpty PullRequest
    -> RIO env (These (NonEmpty PullRequest) (NonEmpty PullRequest))
partitionBase dir base prs = do
    results <- forM prs $ \pr -> do
        result <- gitMergeTree dir ("origin/" <> base) ("origin/" <> pr.prHeadRef)
        case result of
            MergeConflict _ -> do
                logWarn $ "  #" <> display pr.prNumber <> " conflicts with base"
                pure $ Left pr
            MergeOk _ -> pure $ Right pr
    pure $ partitionNE results

-- | For each pair of PRs, test if they conflict when both merged into the base.
-- Uses @git merge-tree@ (in-memory) and temporary commits to avoid touching
-- the working directory.
findConflicts
    :: (HasLogFunc env)
    => FilePath
    -> Text
    -> NonEmpty PullRequest
    -> RIO env (Maybe (NonEmpty ConflictPair))
findConflicts dir base prs = do
    let
        prList = toList prs
        groups = mapMaybe uncons (tails prList)
        total = nPairs (length prList)
    indexRef <- newIORef (0 :: Int)
    fmap (nonEmpty . catMaybes . concat)
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
