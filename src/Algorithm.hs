{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve the postage cost problem.
-}

module Algorithm (
  Solution,
  optimum,
  reprSolution,
  solutionCost,
  resultingInventory,
  almostEqualSol
  ) where

import Data.Foldable ( toList )
import Data.List ( intercalate )
import Data.Sequence ( Seq(..), (<|), (|>), (><) )
import qualified Data.Sequence as S
import Numeric ( showFFloat )
import StampSet

-- | A partial solution of a postage cost problem.
data PartialSolution = Partial Double (Seq StampSet) (Seq StampSet)

-- | A solution of a postage cost problem.
data Solution = Complete Double (Seq StampSet) (Seq StampSet)
  deriving Show

-- | Test if two solutions are equal (float precision: 1e-12). For test purpose
-- only.
almostEqualSol :: Solution -> (Double, Seq StampSet, Seq StampSet) -> Bool
almostEqualSol (Complete t used left) (t', used', left') = cond1 && cond2 && cond3
  where
    cond1 = (abs (t - t') < 1e-12)
    cond2 = almostEqualSeq used used'
    cond3 = almostEqualSeq left left'

-- | Add a stamp set to a partial solution.
add :: PartialSolution -> StampSet -> Int -> PartialSolution
add (Partial c used left) s n = Partial c' used' left'
  where
    c' = c + (price s) * (fromIntegral n)
    (s1, s2) = split s n
    used' = if quantity s1 > 0
      then used |> s1
      else used
    left' = left |> s2

-- | Convert a partial solution to a complete solution.
toCompleteSolution :: PartialSolution -> Seq StampSet -> Solution
toCompleteSolution (Partial t used left) untouched = Complete t used (left >< untouched)

-- | Find the optimal solution of the postage cost problem.
optimum :: Double -> Seq StampSet -> Either String Solution
optimum total inventory = if total <= 0
  then Left "The total cost should be a positive float!"
  else case maybeSol of
    Nothing -> Left "The problem is infeasible!"
    Just sol -> Right sol
  where
    maybeSol = optimum' total (warmUp inventory') (Partial 0 Empty Empty) startingSol
    -- The fact that the sequence is sorted may speed up the algorithm in
    -- some cases.
    inventory' = S.sortBy (\ x y -> compare (-(price x)) (-(price y))) inventory
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
select (Just x) y = if (solutionCost x) < (solutionCost y)
  then Just x
  else Just y

-- | Keep the current solution.
keepCurrent :: Maybe Solution -> PartialSolution -> Bool
keepCurrent Nothing _ = False
keepCurrent (Just current) tmp = (solutionCost current) <= (currentCost tmp)

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
topToBottom total sq = go total sq (Partial 0 Empty Empty)
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
  Just (Complete t used left) -> Just (Complete t (S.reverse used) (S.reverse left))
  where
    msol = topToBottom total (S.reverse sq)

-- | Turn a solution into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation.
reprSolution :: Solution -> String
reprSolution (Complete total used _) = (fmtFloat total) $ " EUR (" ++ stamps ++ ")"
  where
    fmtFloat = showFFloat (Just 2)
    stamps = intercalate ", " (toList (fmap go (S.filter f used)))
    f :: StampSet -> Bool
    f s = (quantity s) > 0
    go :: StampSet -> String
    go s = (show (quantity s)) ++ "x at " ++ (fmtFloat (price s) $ " EUR")

-- | Get the cost of a partial solution.
currentCost :: PartialSolution -> Double
currentCost (Partial c _ _) = c

-- | Get the total cost of a solution.
solutionCost :: Solution -> Double
solutionCost (Complete t _ _) = t

-- | Get the sets of stamps left after solving a problem.
resultingInventory :: Solution -> Seq StampSet
resultingInventory (Complete _ _ left) = left