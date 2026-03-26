module Bach.These
    ( partitionNE
    ) where

import Bach.Prelude
import Data.These (These (..))

-- | Partition a non-empty list of 'Either's. Total: the head seeds
-- the accumulator, then the tail is folded in preserving order.
partitionNE :: NonEmpty (Either a b) -> These (NonEmpty a) (NonEmpty b)
partitionNE (x :| xs) = foldr cons (seed x) xs
  where
    seed (Left a) = This (a :| [])
    seed (Right b) = That (b :| [])

    cons (Left a) (This (ah :| as)) = This (a :| ah : as)
    cons (Left a) (That bs) = These (a :| []) bs
    cons (Left a) (These (ah :| as) bs) = These (a :| ah : as) bs
    cons (Right b) (This as) = These as (b :| [])
    cons (Right b) (That (bh :| bs)) = That (b :| bh : bs)
    cons (Right b) (These as (bh :| bs)) = These as (b :| bh : bs)
