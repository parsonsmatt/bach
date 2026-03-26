module Bach.NonEmptyMap
    ( NonEmptyMap
    , fromMap
    , fromNonEmpty
    , fromNonEmptyWith
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

-- | Build from a non-empty list of key-value pairs.
fromNonEmpty :: (Ord k) => NonEmpty (k, v) -> NonEmptyMap k v
fromNonEmpty nel = NonEmptyMap $ Map.fromList (toList nel)

-- | Build from a non-empty list, combining values for duplicate keys.
fromNonEmptyWith
    :: (Ord k) => (v -> v -> v) -> NonEmpty (k, v) -> NonEmptyMap k v
fromNonEmptyWith f nel = NonEmptyMap $ Map.fromListWith f (toList nel)

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
