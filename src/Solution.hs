{- |
   Module      : Solution
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solution to the postage cost problem.
-}

module Solution (
  PartialSolution(..),
  Solution(..),
  reprSolution,
  reprVariants,
  almostEqualSol,
  emptySolution,
  add,
  toCompleteSolution,
  currentCost,
  currentNStamps,
  solutionNStamps,
  solutionCost,
  resultingInventory
  ) where

import Data.Foldable ( toList )
import Data.List ( intercalate )
import Data.Sequence ( Seq(..), (|>), (><) )
import qualified Data.Sequence as S
import Numeric ( showFFloat )
import StampSet

-- | A partial solution of a postage cost problem.
data PartialSolution = Partial Double Int Double (Seq StampSet) (Seq StampSet)

-- | A solution of a postage cost problem.
data Solution = Complete Double Int Double (Seq StampSet) (Seq StampSet)
  deriving Show

-- | Test if two solutions are equal (float precision: 1e-12). For test purpose
-- only.
almostEqualSol :: Solution -> (Double, Seq StampSet, Seq StampSet) -> Bool
almostEqualSol (Complete _ _ t used left) (t', used', left') = cond1 && cond2 && cond3
  where
    cond1 = (abs (t - t') < 1e-12)
    cond2 = almostEqualSeq used used'
    cond3 = almostEqualSeq left left'

-- | An empty (partial) solution.
emptySolution :: Double -> PartialSolution
emptySolution cost = Partial cost 0 0 Empty Empty

-- | Add a stamp set to a partial solution.
add :: PartialSolution -> StampSet -> Int -> PartialSolution
add (Partial x n c used left) s k = Partial x n' c' used' left'
  where
    c' = c + (price s) * (fromIntegral k)
    (s1, s2) = split s k
    n' = n + quantity s1
    used' = if quantity s1 > 0
      then used |> s1
      else used
    left' = left |> s2

-- | Convert a partial solution to a complete solution.
toCompleteSolution :: PartialSolution -> Seq StampSet -> Solution
toCompleteSolution (Partial x n t used left) untouched = Complete x n t used (left >< untouched)

-- | Turn a solution into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation.
reprSolution :: Solution -> String
reprSolution (Complete _ _ total used _) = (fmtFloat total) $ " EUR (" ++ stamps ++ ")"
  where
    fmtFloat = showFFloat (Just 2)
    stamps = intercalate ", " (toList (fmap go (S.filter f used)))
    f :: StampSet -> Bool
    f s = (quantity s) > 0
    go :: StampSet -> String
    go s = (show (quantity s)) ++ "x at " ++ (fmtFloat (price s) $ " EUR")

-- | Turn a list of variants into a human readable string. See also
-- 'reprSolution.'
reprVariants :: [Solution] -> String
reprVariants sols = if null sols
  then "No variant found!"
  else intercalate "\n" (fmap reprSolution sols)

-- | Get the cost of a partial solution.
currentCost :: PartialSolution -> Double
currentCost (Partial _ _ c _ _) = c

-- | Get the total number of stamps in a partial solution.
currentNStamps :: PartialSolution -> Int
currentNStamps (Partial _ n _ _ _) = n

-- | Get the total number of stamps in a solution.
solutionNStamps :: Solution -> Int
solutionNStamps (Complete _ n _ _ _) = n

-- | Get the total cost of a solution.
solutionCost :: Solution -> Double
solutionCost (Complete _ _ t _ _) = t

-- | Get the sets of stamps left after solving a problem.
resultingInventory :: Solution -> Seq StampSet
resultingInventory (Complete _ _ _ _ left) = left
