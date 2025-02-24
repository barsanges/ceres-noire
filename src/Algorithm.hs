{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2022 barsanges
   License     : GNU GPL, version 3

Find the sets of stamps whose total value lies within the given range.
-}

module Algorithm (
  withinRange,
  dropSupersets,
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
  | otherwise = if null res
                then Left "The problem is infeasible!"
                else Right (Seq.sort (Seq.fromList res))
    where
      tmp = solve mn (low - 1e-9) (up + 1e-9) (collectionToList inventory)
      res = fmap (foldr add (emptyLike inventory)) tmp

-- | The function that actually search the sets of stamps whose total
-- value lies within the given range. Do not perform any check
-- regarding the validity of the inputs.
solve :: Maybe Int -> Double -> Double -> [StampSet] -> [[StampSet]]
solve mn low up inventory = filterBelow low (filter (\ x -> fst x /= 0) $ findAbove mn up inventory)

-- | Filter the pairs whose first value is below a given threshold.
filterBelow :: Ord a => a -> [(a, [b])] -> [[b]]
filterBelow threshold = go
  where
    go [] = []
    go ((cost, xs):yss) | cost >= threshold = xs:(go yss)
                        | otherwise = go yss

-- | Find the combinations of stamps whose total value is above a
-- given threshold. Each combination should contain less than a given
-- number of stamps.
findAbove :: Maybe Int -> Double -> [StampSet] -> [(Double, [StampSet])]
findAbove _ _ [] = [(0, [])]
findAbove (Just 0) _ _ = [(0, [])]
findAbove mn up (x:ys) | up < 0 = [(0, [])]
                       | otherwise = concatMap go [0..imax]
  where
    imax :: Int
    imax = case mn of
             Nothing -> min (floor (up / (price x))) (quantity x)
             Just n -> min n (min (floor (up / (price x))) (quantity x))
    go :: Int -> [(Double, [StampSet])]
    go i = [ (cost + current, (changeQuantity x i):zs)
           | (cost, zs) <- findAbove mn' up' ys ]
      where
        mn' = mn `mminus` i
        current = (price x) * (fromIntegral i)
        up' = up - current

-- | Substract a value to a Maybe value.
mminus :: Num a => Maybe a -> a -> Maybe a
mminus Nothing _ = Nothing
mminus (Just x) y = Just (x - y)

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
