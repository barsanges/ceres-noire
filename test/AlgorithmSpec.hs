{- |
   Module      : AlgorithmSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Algorithm.
-}

module AlgorithmSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import Data.Either ( isRight )
import Data.Maybe ( fromJust )
import Data.Sequence ( Seq(..), fromList )

import Algorithm
import StampSet

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (fromJust (mkStampSet (1e-9 + abs p) (abs q)))

s1 :: StampSet
s1 = fromJust (mkStampSet 1.05 3)

s2 :: StampSet
s2 = fromJust (mkStampSet 1.82 1)

s3 :: StampSet
s3 = fromJust (mkStampSet 0.05 4)

sq1 :: Seq StampSet
sq1 = fromList [s1, s2, s3]

sq2 :: Seq StampSet
sq2 = fromList [fromJust (mkStampSet 3 5),
                fromJust (mkStampSet 1 5),
                fromJust (mkStampSet 0.5 10)]

type PseudoSol = (Double, Seq StampSet, Seq StampSet)

-- FIXME: share some code between the two test modules.
eitherEqual :: Either String Solution -> Either String PseudoSol -> Bool
eitherEqual (Left x) (Left y) = x == y
eitherEqual (Right _) (Left _) = False
eitherEqual (Left _) (Right _) = False
eitherEqual (Right x) (Right y) = x `almostEqualSol` y

eitherEquals :: Either String [Solution] -> Either String [PseudoSol] -> Bool
eitherEquals (Left x) (Left y) = x == y
eitherEquals (Right _) (Left _) = False
eitherEquals (Left _) (Right _) = False
eitherEquals (Right x) (Right y) = if (length x) == (length y)
  then and (zipWith almostEqualSol x y)
  else False

sol1 :: PseudoSol
sol1 = (1.10,
        fromList [fromJust (mkStampSet 1.05 1),
                  fromJust (mkStampSet 0.05 1)],
        fromList [fromJust (mkStampSet 1.82 1),
                  fromJust (mkStampSet 1.05 2),
                  fromJust (mkStampSet 0.05 3)])

sol2 :: PseudoSol
sol2 = (3.5,
        fromList [fromJust (mkStampSet 3 1),
                  fromJust (mkStampSet 0.5 1)],
        fromList [fromJust (mkStampSet 3 4),
                  fromJust (mkStampSet 1 5),
                  fromJust (mkStampSet 0.5 9)])

sol3 :: PseudoSol
sol3 = (2,
        fromList [fromJust (mkStampSet 1 2)],
        fromList [fromJust (mkStampSet 3 5),
                  fromJust (mkStampSet 1 3),
                  fromJust (mkStampSet 0.5 10)])

var1 :: PseudoSol
var1 = (3.5,
        fromList [fromJust (mkStampSet 1 3),
                  fromJust (mkStampSet 0.5 1)],
        fromList [fromJust (mkStampSet 3 5),
                  fromJust (mkStampSet 1 2),
                  fromJust (mkStampSet 0.5 9)])

var2 :: PseudoSol
var2 = (4,
        fromList [fromJust (mkStampSet 3 1),
                  fromJust (mkStampSet 1 1)],
        fromList [fromJust (mkStampSet 3 4),
                  fromJust (mkStampSet 1 4),
                  fromJust (mkStampSet 0.5 10)])

var3 :: PseudoSol
var3 = (2,
        fromList [fromJust (mkStampSet 1 1),
                  fromJust (mkStampSet 0.5 2)],
        fromList [fromJust (mkStampSet 3 5),
                  fromJust (mkStampSet 1 4),
                  fromJust (mkStampSet 0.5 8)])

probGen :: Int -> Gen (Double, Seq StampSet)
probGen n = do
  x <- arbitrary
  y <- resize n (fmap fromList (listOf arbitrary))
  return (x, y)

propTotalCost (x, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    x' = 0.01 + abs x
    res = optimum x' inventory
    ok = isRight res
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right sol -> solutionCost sol >= x'

propResultingInventory (x, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    x' = 0.01 + abs x
    res = optimum x' inventory
    ok = isRight res
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right sol -> foldr go True (resultingInventory sol)
      where
        go :: StampSet -> Bool -> Bool
        go s b = b && (quantity s >= 0)

spec :: Spec
spec = do
  describe "optimum" $ do
    it "choose the least costly combination of stamps for a letter" $
      ((optimum 1.06 sq1) `eitherEqual` (Right sol1)) `shouldBe` True

    it "choose the optimal combination with the least number of stamps (1)" $
      ((optimum 3.5 sq2) `eitherEqual` (Right sol2)) `shouldBe` True

    it "choose the optimal combination with the least number of stamps (2)" $
      ((optimum 2 sq2) `eitherEqual` (Right sol3)) `shouldBe` True

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      ((optimum 6 sq1) `eitherEqual` (Left "The problem is infeasible!")) `shouldBe` True

    it "should fail if the cost is zero" $
      ((optimum 0 sq1) `eitherEqual` (Left "The total cost should be a positive float!")) `shouldBe` True

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((optimum (0.001 + abs x) Empty) `eitherEqual` (Left "The problem is infeasible!"))

    it "should always fail if the total cost < 0" $ property $
      \ x -> ((optimum (-(abs x)) Empty) `eitherEqual` (Left "The total cost should be a positive float!"))

    it "should always give a solution (if it exists) with a cost greater or equal to the cost of the letter" $ property $
      forAll (probGen 5) propTotalCost

    it "should never return an inventory with a negative number of stamps" $ property $
      forAll (probGen 5) propResultingInventory
