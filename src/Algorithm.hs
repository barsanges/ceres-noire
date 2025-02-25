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
import Data.List ( intercalate, sort )
import Stamps

-- | Filter the sets that are supersets of other sets in the sequence.
dropSupersets :: [Collection] -> [Collection]
dropSupersets xs = filter (noStrictSubset xs) xs

-- | Find the sets of stamps whose total value lies within the given range.
withinRange :: Double
            -> Maybe Int
            -> Double
            -> Double
            -> Collection
            -> Either String [Collection]
withinRange tolerance mn low up inventory
  | low < 0 = Left "The minimum value should be a positive float!"
  | up < low = Left "The maximum value should be greater than the minimum value!"
  | any (\ n -> n <= 0) mn = Left "The maximal number of stamps should be strictly positive!"
  | otherwise = if null res
                then Left "The problem is infeasible!"
                else Right (sort res)
    where
      tmp = solve mn (low - tolerance) (up + tolerance) (collectionToList inventory)
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

-- | Turn a list of collections into a human readable string. See also
-- 'reprCollection'.
reprCollections :: [Collection] -> String
reprCollections seqs = intercalate "\n" (toList $ fmap reprCollection seqs)
