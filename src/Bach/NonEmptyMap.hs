module Bach.NonEmptyMap
    ( NonEmptyMap
    , fromMap
    , singleton
    , toNonEmpty
    , toMap
    ) where

import Bach.Prelude
import qualified RIO.Map as Map

-- | A 'Map' guaranteed to contain at least one entry.
-- The constructor is not exported; use 'fromMap' or 'singleton'.
newtype NonEmptyMap k v = NonEmptyMap (Map.Map k v)
    deriving stock (Show, Eq)
    deriving newtype (Foldable, Semigroup)

-- | Wrap a 'Map' if non-empty.
fromMap :: Map.Map k v -> Maybe (NonEmptyMap k v)
fromMap m
    | Map.null m = Nothing
    | otherwise = Just (NonEmptyMap m)

-- | A map with exactly one entry.
singleton :: k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap (Map.singleton k v)

-- | All entries, ascending by key. Guaranteed non-empty.
toNonEmpty :: NonEmptyMap k v -> NonEmpty (k, v)
toNonEmpty (NonEmptyMap m) =
    case Map.toAscList m of
        (x : xs) -> x :| xs
        [] -> error "NonEmptyMap: invariant violated"

-- | Unwrap to a regular 'Map'.
toMap :: NonEmptyMap k v -> Map.Map k v
toMap (NonEmptyMap m) = m
