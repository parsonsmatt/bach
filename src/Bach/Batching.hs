module Bach.Batching
    ( buildBatches
    ) where

import Bach.Prelude
import Bach.Types
import RIO.List (headMaybe)
import qualified RIO.Map as Map
import qualified RIO.Set as Set

-- | Greedy graph coloring: assign each PR to the lowest batch number
-- not used by any conflicting neighbor.
buildBatches :: [PullRequest] -> Set.Set (Int, Int) -> Map.Map Int [PullRequest]
buildBatches prs conflicts =
    let
        adjMap :: Map.Map Int (Set.Set Int)
        adjMap =
            Map.fromListWith (<>)
                $ concatMap pairToEdges (Set.toList conflicts)

        pairToEdges (left, right) =
            [(left, Set.singleton right), (right, Set.singleton left)]

        assignments = foldl' assign Map.empty prs

        assign assigned pr =
            let
                neighbors = fromMaybe Set.empty $ Map.lookup pr.prNumber adjMap
                usedBatches = foldMap Set.singleton (Map.restrictKeys assigned neighbors)
                batch = firstAvailableBatch usedBatches
             in
                Map.insert pr.prNumber batch assigned
     in
        Map.fromListWith (flip (<>))
            $ mapMaybe (assignmentEntry assignments) prs

assignmentEntry :: Map.Map Int Int -> PullRequest -> Maybe (Int, [PullRequest])
assignmentEntry assignments pr = do
    batch <- Map.lookup pr.prNumber assignments
    pure (batch, [pr])

-- | Find the lowest batch number (starting from 1) not in the given set.
firstAvailableBatch :: Set.Set Int -> Int
firstAvailableBatch used =
    fromMaybe 1 $ headMaybe $ filter (`Set.notMember` used) [1 ..]
