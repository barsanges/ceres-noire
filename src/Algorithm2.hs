{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Find the sets of stamps whose total value lies within the given range.
-}

module Algorithm2 (
  withinRange,
  dropSupersets
  ) where

import Data.Foldable ( toList )
import Data.Sequence ( Seq(..), (<|) )
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as Set

import StampSet

type Collection = Seq StampSet -- FIXME: find another name?

-- | Filter the sets that are supersets of other sets in the sequence.
dropSupersets :: Seq Collection -> Seq Collection
dropSupersets xs = Seq.filter (noStrictSubset xs) xs

-- | Find the sets of stamps whose total value lies within the given range.
withinRange :: Double -> Double -> Collection -> Either String (Seq Collection)
withinRange low up inventory
  | low < 0 = Left "The minimum value should be a positive float!"
  | up < low = Left "The maximum value should be greater than the minimum value!"
  | otherwise = if Set.null res
                then Left "The problem is infeasible!"
                else Right (Seq.sortBy comp (Seq.fromList . toList $ res))
    where
      res = solve low up inventory

-- | The function that actually search the sets of stamps whose total value lies
-- within the given range. Do not perform any check regarding the validity of
-- the inputs.
solve :: Double -> Double -> Collection -> Set Collection
solve low up inventory = go inventory (Set.singleton Empty)
  where
    go :: Collection -> Set Collection -> Set Collection
    go Empty _ = Set.empty
    go (s :<| ss) partial = Set.union complete complete'
      where
        cols = Set.unions (Set.map (mkCollectionRange s) partial)
        (partial', complete) = sieve low up cols
        complete' = go ss partial'

-- | Create all collections resulting from the union of `col` and the
-- elements of `mkRange s`.
mkCollectionRange :: StampSet -> Collection -> Set Collection
mkCollectionRange s col = Set.map go (Set.fromList . toList $ mkRange s)
  where
    go :: StampSet -> Collection
    go s' = if quantity s' > 0
            then s' <| col
            else col

-- | Sort the given collections, and return on one hand the ones whose
-- total value is lesser than `up`, and on the other hand the ones
-- whose total value lies between `low` and `up`. All collections
-- belonging to the second sequence belong also to the first one.
sieve :: Double -> Double -> Set Collection -> (Set Collection, Set Collection)
sieve low up = foldr go (Set.empty, Set.empty)
  where
    go :: Collection
       -> (Set Collection, Set Collection)
       -> (Set Collection, Set Collection)
    go xs (tmp, res)
      | totalValue xs > up = (tmp, res)
      | totalValue xs < low = (Set.insert xs tmp, res)
      | otherwise = (Set.insert xs tmp, Set.insert xs res)

-- | Compare two collections: a collection is "smaller" than the other
-- if it is less costly or, if they have the same value, if it
-- contains less stamps. If they have the same cost and contain the
-- same number of stamps, they are considered equal (even if they do
-- not contain the same stamps!).
comp :: Collection -> Collection -> Ordering
comp xs ys
  | val == LT = LT
  | val == EQ = qty
  | otherwise = GT
  where
    val = compare (totalValue xs) (totalValue ys)
    qty = compare (totalQuantity xs) (totalQuantity ys)

-- | `noSubset ss s` returns `True` if `s` has no strict subset in `ss`.
noStrictSubset :: Seq Collection -> Collection -> Bool
noStrictSubset ss s = not (any (isStrictSubset' s) ss)

-- | `isStrictSubset s s'` returns `True` if `s` is a strict subset of `s'`.
isStrictSubset :: Collection -> Collection -> Bool
isStrictSubset s s' = all go s
  where
    go :: StampSet -> Bool
    go x = any og s'
      where
        og :: StampSet -> Bool
        og y = (price x == price y) && (quantity x < quantity y)

-- | Same as `isStrictSubset`, but with the arguments reversed:
-- `isStrictSubset' s' s` returns `True` if `s` is a strict subset of
-- `s'`.
isStrictSubset' :: Collection -> Collection -> Bool
isStrictSubset' = flip isStrictSubset