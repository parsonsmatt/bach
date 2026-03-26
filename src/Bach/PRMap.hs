module Bach.PRMap
    ( PRMap
    , empty
    , insert
    , member
    , elems
    ) where

import Bach.Prelude
import Bach.Types
import qualified RIO.Map as Map

-- | A map from PR number to 'PullRequest', keyed by 'prNumber'.
-- Duplicate inserts keep the first value.
newtype PRMap = PRMap (Map.Map Int PullRequest)
    deriving stock (Show, Eq)

empty :: PRMap
empty = PRMap Map.empty

-- | Insert a PR. If a PR with the same number already exists, keeps the
-- existing one. Returns 'True' in the second element if the PR was new.
insert :: PullRequest -> PRMap -> (Bool, PRMap)
insert pr (PRMap m)
    | Map.member pr.prNumber m = (False, PRMap m)
    | otherwise = (True, PRMap (Map.insert pr.prNumber pr m))

-- | Check if a PR number is in the map.
member :: Int -> PRMap -> Bool
member n (PRMap m) = Map.member n m

-- | All PRs in the map. Non-empty if at least one PR was inserted.
elems :: PRMap -> [PullRequest]
elems (PRMap m) = Map.elems m
