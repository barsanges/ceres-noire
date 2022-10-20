{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve the postage cost problem.
-}

module Algorithm (
  module Solution,
  optimum
  ) where

import Data.Sequence ( Seq(..), (<|) )
import qualified Data.Sequence as S

import Solution
import StampSet

-- | Find the optimal solution of the postage cost problem.
optimum :: Double -> Seq StampSet -> Either String Solution
optimum total inventory = if total <= 0
  then Left "The total cost should be a positive float!"
  else case maybeSol of
    Nothing -> Left "The problem is infeasible!"
    Just sol -> Right sol
  where
    maybeSol = optimum' total (warmUp inventory') (emptySolution total) startingSol
    -- The fact that the sequence is sorted may speed up the algorithm in
    -- some cases.
    inventory' = reorder inventory
    startingSol = initialize total inventory'

-- | Recursively find an optimal solution.
optimum' :: Double -> Seq (Double, StampSet) -> PartialSolution -> Maybe Solution -> Maybe Solution
optimum' total Empty tmp best = if total <= 0
  then select best (toCompleteSolution tmp Empty)
  else best
optimum' total ((v, s) :<| stampSets) tmp best
  | keepCurrent best tmp = best
  | total > setValue s + v = best
  | otherwise = foldr go best [0..(min q n)]
  where
    p = price s
    q = quantity s
    n = ceiling (total / p)
    go :: Int -> Maybe Solution -> Maybe Solution
    go k b = if total' < 0
      then select b (toCompleteSolution tmp' (fmap snd stampSets))
      else optimum' total' stampSets tmp' b
      where
        total' = total - p * (fromIntegral k)
        tmp' = add tmp s k

-- | Select the best solution.
select :: Maybe Solution -> Solution -> Maybe Solution
select Nothing y = Just y
select (Just x) y
  | abs (solutionCost x - solutionCost y) < 1e-12 = if (solutionNStamps x) <= (solutionNStamps y)
    then Just x
    else Just y
  | (solutionCost x) < (solutionCost y) = Just x
  | otherwise = Just y

-- | Keep the current solution.
keepCurrent :: Maybe Solution -> PartialSolution -> Bool
keepCurrent Nothing _ = False
keepCurrent (Just current) tmp =
  if abs (solutionCost current - currentCost tmp) < 1e-12
  then (solutionNStamps current) < (currentNStamps tmp)
  else (solutionCost current) < (currentCost tmp)

-- | Associate the total value of all following sets to each stamp set of the
-- sequence.
warmUp :: Seq StampSet -> Seq (Double, StampSet)
warmUp sq = snd (foldr go (0, Empty) sq)
  where
    go :: StampSet -> (Double, Seq (Double, StampSet)) -> (Double, Seq (Double, StampSet))
    go x (t, s) = (t + setValue x, (t, x) <| s)

-- | Find a simple solution (likely suboptimal, but hopefully not too much) to
-- a given problem. It will be used as an upper bound for the optimization.
initialize :: Double -> Seq StampSet -> Maybe Solution
initialize total collection = case s1 of
  Nothing -> Nothing -- The problem is infeasible.
  Just sol -> select s2 sol
  where
    s1 = topToBottom total collection
    s2 = bottomToTop total collection

-- | Find a solution (if any) by choosing the most costly stamps first.
topToBottom :: Double -> Seq StampSet -> Maybe Solution
topToBottom total sq = go total sq (emptySolution total)
  where
    go :: Double -> Seq StampSet -> PartialSolution -> Maybe Solution
    go t Empty tmp = if t <= 0
      then Just (toCompleteSolution tmp Empty)
      else Nothing
    go t (s :<| stampSets) tmp
      | t <= 0 = Just (toCompleteSolution tmp (s <| stampSets))
      | t' <= 0 = Just (toCompleteSolution tmp1 stampSets)
      | otherwise = go t' stampSets tmp2
      where
        t' = t - setValue s
        n = ceiling (t / (price s))
        tmp1 = add tmp s n
        tmp2 = add tmp s (quantity s)

-- | Find a solution (if any) by choosing the least costly stamps first.
bottomToTop :: Double -> Seq StampSet -> Maybe Solution
bottomToTop total sq = case msol of
  Nothing -> Nothing
  Just (Complete c n t used left) -> Just (Complete c n t (S.reverse used) (S.reverse left))
  where
    msol = topToBottom total (S.reverse sq)

-- | Reorder a sequence of stamps sets by decreasing prices.
reorder :: Seq StampSet -> Seq StampSet
reorder = S.sortOn (\ x -> -(price x))
