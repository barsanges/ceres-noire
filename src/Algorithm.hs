{- |
   Module      : Algorithm
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Solve the postage cost problem.
-}

module Algorithm (
  Solution,
  optimum,
  variants,
  reprSolution,
  reprVariants,
  solutionCost,
  resultingInventory,
  almostEqualSol
  ) where

import Data.Foldable ( toList )
import Data.List ( intercalate )
import Data.Maybe ( catMaybes )
import Data.Sequence ( Seq(..), (<|), (|>), (><) )
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
keepCurrent (Just current) tmp = (solutionCost current) < (currentCost tmp)

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

-- | Find suboptimal solutions close (in terms of stamps) to the given one.
variants :: Solution -> [Solution]
variants (Complete cost _ _ used left) = catMaybes (fmap go (popEach Empty used))
  where
    toSolution :: Seq StampSet -> Seq StampSet -> Solution
    toSolution u l = Complete cost (totalQuantity u) (totalValue u) (reorder u) (reorder l)

    popEach :: Seq StampSet -> Seq StampSet -> [(StampSet, Seq StampSet)]
    popEach _ Empty = []
    popEach xs (y :<| ys) = (y, xs >< ys):(popEach (xs |> y) ys)

    go :: (StampSet, Seq StampSet) -> Maybe Solution
    go (x, xs) = case optimum (cost - (totalValue xs')) left' of
      Left _ -> Nothing
      Right (Complete _ _ _ u l) -> let u' = simplify (xs' >< u)
                                        l' = simplify (x1 <| xlike >< l)
                                    in Just (toSolution u' l')
      where
        (x1, x2) = split x 1
        xs' = x2 <| xs
        (xlike, left') = S.partition (\ a -> abs (price a - price x) < 1e-12) left

-- | Reorder a sequence of stamps sets by decreasing prices.
reorder :: Seq StampSet -> Seq StampSet
reorder = S.sortOn (\ x -> -(price x))

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

-- | Get the total number of stamps in a solution.
solutionNStamps :: Solution -> Int
solutionNStamps (Complete _ n _ _ _) = n

-- | Get the total cost of a solution.
solutionCost :: Solution -> Double
solutionCost (Complete _ _ t _ _) = t

-- | Get the sets of stamps left after solving a problem.
resultingInventory :: Solution -> Seq StampSet
resultingInventory (Complete _ _ _ _ left) = left