module Bach.Batching
    ( buildBatches
    , selectBatch
    , NoBatchEligible (..)
    ) where

import Bach.NonEmptyMap (NonEmptyMap)
import qualified Bach.NonEmptyMap as NEM
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
-- not used by any conflicting neighbor. Every PR in the non-empty input
-- is assigned exactly one batch, so the output is guaranteed non-empty.
buildBatches
    :: NonEmpty PullRequest
    -> Set.Set (Int, Int)
    -> NonEmptyMap Int (NonEmpty PullRequest)
buildBatches prs conflicts =
    NEM.fromNonEmptyWith (flip (<>)) assignments
  where
    prList = toList prs

    adjMap :: Map.Map Int (Set.Set Int)
    adjMap =
        Map.fromListWith (<>)
            $ concatMap pairToEdges (Set.toList conflicts)

    pairToEdges (left, right) =
        [(left, Set.singleton right), (right, Set.singleton left)]

    batchOf = foldl' assign Map.empty prList

    assign assigned pr =
        let
            neighbors = fromMaybe Set.empty $ Map.lookup pr.prNumber adjMap
            usedBatches = foldMap Set.singleton (Map.restrictKeys assigned neighbors)
            batch = firstAvailableBatch usedBatches
         in
            Map.insert pr.prNumber batch assigned

    assignments = fmap assignEntry prs

    assignEntry pr =
        let
            batch = fromMaybe 1 $ Map.lookup pr.prNumber batchOf
         in
            (batch, pr :| [])

-- | Find the lowest batch number (starting from 1) not in the given set.
firstAvailableBatch :: Set.Set Int -> Int
firstAvailableBatch used =
    fromMaybe 1 $ headMaybe $ filter (`Set.notMember` used) [1 ..]

-- | Select the best batch: the largest one containing all must-include PRs.
-- When must-include is empty, just picks the largest batch.
-- Returns (selected batch, all other PRs), or Left if no batch is eligible.
selectBatch
    :: Set.Set Int
    -> NonEmptyMap Int (NonEmpty PullRequest)
    -> Either NoBatchEligible (NonEmpty PullRequest, [PullRequest])
selectBatch mustInclude batches =
    let
        batchList = toList $ NEM.toNonEmpty batches
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
                        concatMap (toList . snd)
                            $ filter (\(k, _) -> k /= chosenKey) batchList
                 in
                    Right (chosen, deferred)
  where
    containsMustInclude prs =
        let
            prNums = Set.fromList $ toList $ fmap (.prNumber) prs
         in
            Set.isSubsetOf mustInclude prNums
