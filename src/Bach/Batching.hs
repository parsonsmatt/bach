module Bach.Batching
    ( buildBatches
    , selectBatch
    , NoBatchEligible (..)
    ) where

import Bach.Prelude
import Bach.Types
import Data.List (intercalate)
import RIO.List (headMaybe, sortOn)
import qualified RIO.Map as Map
import qualified RIO.Set as Set

data NoBatchEligible = NoBatchEligible !(Set.Set Int)
    deriving stock (Show, Eq)

instance Exception NoBatchEligible where
    displayException (NoBatchEligible required) =
        "No batch contains all must-include PRs: #"
            <> intercalate ", #" (map show (Set.toList required))

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

-- | Select the best batch: the largest one containing all must-include PRs.
-- When must-include is empty, just picks the largest batch.
-- Returns (selected batch, all other PRs), or Left if no batch is eligible.
selectBatch
    :: Set.Set Int
    -> Map.Map Int [PullRequest]
    -> Either NoBatchEligible ([PullRequest], [PullRequest])
selectBatch mustInclude batches
    | Map.null batches = Right ([], [])
    | otherwise =
        let
            batchList = Map.toList batches
            eligible =
                filter (containsMustInclude . snd) batchList
            sorted =
                sortOn (Down . length . snd) eligible
         in
            case sorted of
                [] ->
                    Left $ NoBatchEligible mustInclude
                ((chosenKey, chosen) : _) ->
                    let
                        deferred =
                            concatMap snd
                                $ filter (\(k, _) -> k /= chosenKey) batchList
                     in
                        Right (chosen, deferred)
  where
    containsMustInclude prs =
        let
            prNums = Set.fromList $ map (.prNumber) prs
         in
            Set.isSubsetOf mustInclude prNums
