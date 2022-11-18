{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Find the sets of stamps whose total value lies within the given range.
-}

module Algorithm (
  withinRange,
  dropSupersets,
  reprCollection,
  reprCollections
  ) where

import Data.Foldable ( toList )
import Data.List ( intercalate )
import Data.Sequence ( Seq(..) )
import qualified Data.Sequence as Seq
import Data.Set ( Set )
import qualified Data.Set as Set

import Stamps

-- | Filter the sets that are supersets of other sets in the sequence.
dropSupersets :: Seq Collection -> Seq Collection
dropSupersets xs = Seq.filter (noStrictSubset xs) xs

-- | Find the sets of stamps whose total value lies within the given range.
withinRange :: Maybe Int -> Double -> Double -> Collection -> Either String (Seq Collection)
withinRange mn low up inventory
  | low < 0 = Left "The minimum value should be a positive float!"
  | up < low = Left "The maximum value should be greater than the minimum value!"
  | any (\ n -> n <= 0) mn = Left "The maximal number of stamps should be strictly positive!"
  | otherwise = if Set.null res
                then Left "The problem is infeasible!"
                else Right (Seq.sort (Seq.fromList . toList $ res))
    where
      res = solve mn low up inventory

-- | The function that actually search the sets of stamps whose total value lies
-- within the given range. Do not perform any check regarding the validity of
-- the inputs.
solve :: Maybe Int -> Double -> Double -> Collection -> Set Collection
solve mn low up inventory = res
  where
    (_, res) = crease go (Set.singleton (emptyLike inventory), Set.empty) inventory

    go :: StampSet
       -> (Set Collection, Set Collection)
       -> (Set Collection, Set Collection)
    go s (partial, complete) = (partial', Set.union complete complete')
      where
        cols = Set.unions (Set.map (mkCollectionRange s) partial)
        (partial', complete') = sieve mn low up cols

-- | Create all collections resulting from the union of `col` and the
-- elements of `mkRange s`.
mkCollectionRange :: StampSet -> Collection -> Set Collection
mkCollectionRange s col = Set.map go (Set.fromList (mkRange s))
  where
    go :: StampSet -> Collection
    go s' = add s' col

-- | Sort the given collections, and return on one hand the ones whose
-- total value is lesser than `up`, and on the other hand the ones
-- whose total value lies between `low` and `up`. All collections
-- belonging to the second sequence belong also to the first one.
sieve :: Maybe Int -> Double -> Double -> Set Collection -> (Set Collection, Set Collection)
sieve mn low up = foldr go (Set.empty, Set.empty)
  where
    go :: Collection
       -> (Set Collection, Set Collection)
       -> (Set Collection, Set Collection)
    go xs (tmp, res)
      | any (\ n -> totalQuantity xs > n) mn = (tmp, res)
      | totalValue xs > up = (tmp, res)
      | totalValue xs < low = (Set.insert xs tmp, res)
      | otherwise = (Set.insert xs tmp, Set.insert xs res)

-- | Turn a list of collections into a human readable string. See also
-- 'reprCollection'.
reprCollections :: Seq Collection -> String
reprCollections seqs = intercalate "\n" (toList $ fmap reprCollection seqs)
