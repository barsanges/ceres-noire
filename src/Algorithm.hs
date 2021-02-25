{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve the postage cost problem.
-}

module Algorithm (
  Solution,
  boundary,
  reprBoundary,
  optimum,
  reprSolution,
  solutionCost,
  resultingInventory,
  almostEqualSol,
  almostEqualBoundary
  ) where

import Data.Foldable ( toList )
import Data.List ( intercalate )
import qualified Data.List as L
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

-- | Test if two lists of solutions are equal (float precision: 1e-12). For
-- test purpose only.
almostEqualBoundary :: [Solution] -> [(Double, Seq StampSet, Seq StampSet)] -> Bool
almostEqualBoundary x y = if (length x) == (length y)
  then foldr (&&) True (zipWith almostEqualSol x y)
  else False

-- | Add a stamp set to a partial solution.
add :: PartialSolution -> StampSet -> Int -> PartialSolution
add (Partial c used left) s n = Partial c' (used |> s1) (left |> s2)
  where
    c' = c + (price s) * (fromIntegral n)
    (s1, s2) = split s n

-- | Convert a partial solution to a complete solution.
toCompleteSolution :: PartialSolution -> Seq StampSet -> Solution
toCompleteSolution (Partial t used left) untouched = Complete t used (left >< untouched)

-- | Find the boundary of the set of admissible solutions.
boundary :: Double -> Seq StampSet -> Either String [Solution]
boundary total inventory = if total <= 0
  then Left "The total cost should be a positive float!"
  else if null sols
    then Left "The problem is infeasible!"
    else Right (L.sortBy go sols)
  where
    sols = boundary' total inventory' (Partial 0 Empty Empty)
    go :: Solution -> Solution -> Ordering
    go (Complete x _ _) (Complete y _ _) = compare x y
    -- The fact that the sequence is sorted may speed up the algorithm in
    -- some cases.
    inventory' = S.sortBy (\ x y -> compare (-(price x)) (-(price y))) inventory

-- | Recursively find the boundary of the set of admissible solutions.
boundary' :: Double -> Seq StampSet -> PartialSolution -> [Solution]
boundary' _ Empty _ = []
boundary' total (s :<| stampSets) tmp = case stampSets of
  Empty -> if n <= q
    then [toCompleteSolution (add tmp s n) Empty]
    else []
  _ -> concat [ go i | i <- [0..(min q n)] ] -- FIXME: use something else than a list?
  where
    p = price s
    q = quantity s
    n = ceiling (total / p)
    go :: Int -> [Solution]
    go k = if total' < 0
      then [toCompleteSolution tmp' stampSets]
      else boundary' total' stampSets tmp'
      where
        total' = total - p * (fromIntegral k)
        tmp' = add tmp s k

-- | Find the optimal solution of the postage cost problem.
optimum :: Double -> Seq StampSet -> Either String Solution
optimum total inventory = case res of
  Left msg -> Left msg
  Right b -> case b of
    [] -> Left "The problem is infeasible!"
    (x:_) -> Right x
  where
    -- With a specific algorithm for the optimum, it would be possible to stop
    -- the exploration of branches when they appear suboptimal.
    res = boundary total inventory

-- | Associate the total value of all following sets to each stamp set of the
-- sequence.
warmUp :: Seq StampSet -> Seq (Double, StampSet)
warmUp sq = snd (foldr go (0, Empty) sq)
  where
    go :: StampSet -> (Double, Seq (Double, StampSet)) -> (Double, Seq (Double, StampSet))
    go x (t, s) = (t + setValue x, (t, x) <| s)

-- | Turn a solution into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation.
reprSolution :: Solution -> String
reprSolution (Complete total used _) = (fmtFloat total) $ " EUR (" ++ stamps ++ ")"
  where
    fmtFloat = showFFloat (Just 2)
    stamps = intercalate ", " (toList (fmap go used))
    go :: StampSet -> String
    go s = (show (quantity s)) ++ "x at " ++ (fmtFloat (price s) $ " EUR")

-- | Turn a list of solutions into a human readable string. The resulting string
-- is not exhaustive and should not be used for serialisation.
reprBoundary :: [Solution] -> String
reprBoundary xs = intercalate "\n" (fmap reprSolution xs)

-- | Get the cost of a partial solution.
currentCost :: PartialSolution -> Double
currentCost (Partial c _ _) = c

-- | Get the total cost of a solution.
solutionCost :: Solution -> Double
solutionCost (Complete t _ _) = t

-- | Get the sets of stamps left after solving a problem.
resultingInventory :: Solution -> Seq StampSet
resultingInventory (Complete _ _ left) = left