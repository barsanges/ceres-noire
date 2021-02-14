{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve the postage cost problem.
-}

module Algorithm (
  boundary,
  reprBoundary,
  optimum,
  reprSolution,
  resultingInventory
  ) where

import Data.Foldable ( toList )
import Data.List ( minimumBy, intercalate )
import Data.Sequence ( Seq(..), (|>), (><), sortBy )
import Numeric ( showFFloat )
import StampSet

-- | A partial solution of a postage cost problem.
data PartialSolution = Partial (Seq StampSet) (Seq StampSet)

-- | A solution of a postage cost problem.
data Solution = Complete Float (Seq StampSet) (Seq StampSet)

-- | Add a stamp set to a partial solution.
add :: PartialSolution -> StampSet -> Int -> PartialSolution
add (Partial used left) s n = Partial (used |> s1) (left |> s2)
  where
    (s1, s2) = split s n

-- | Convert a partial solution to a complete solution.
toCompleteSolution :: PartialSolution -> Seq StampSet -> Solution
toCompleteSolution (Partial used left) untouched = Complete total used (left >< untouched)
  where
    total = sum (fmap go used)
    go :: StampSet -> Float
    go s = price s * (fromIntegral . quantity $ s)

-- | Find the boundary of the set of admissible solutions.
boundary :: Float -> Seq StampSet -> Either String [Solution]
boundary total inventory = if total <= 0
  then Left "The total cost should be a positive float!"
  else Right (boundary' total inventory' (Partial Empty Empty))
  where
    -- The fact that the sequence is sorted may speed up the algorithm in
    -- some cases.
    inventory' = sortBy (\ x y -> compare (price x) (price y)) inventory

-- | Recursively find the boundary of the set of admissible solutions.
boundary' :: Float -> Seq StampSet -> PartialSolution -> [Solution]
boundary' _ Empty _ = []
boundary' total (s :<| stampSets) tmp = case stampSets of
  Empty -> if n <= q
    then [toCompleteSolution (add tmp s n) Empty]
    else []
  _ -> concat [ go i | i <- [0..n] ] -- FIXME: use something else than a list?
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
optimum :: Float -> Seq StampSet -> Either String Solution
optimum total inventory = case res of
  Left msg -> Left msg
  Right b -> if null b
    then Left "The problem is infeasible!"
    else Right $ minimumBy go b
  where
    res = boundary total inventory -- FIXME: define a specific algorithm for the optimum.
    go :: Solution -> Solution -> Ordering
    go (Complete x _ _) (Complete y _ _) = compare x y

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

-- | Get the sets of stamps left after solving a problem.
resultingInventory :: Solution -> Seq StampSet
resultingInventory (Complete _ _ left) = left