{- |
   Module      : AlgorithmSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Algorithm.
-}

module AlgorithmSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck

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
  then foldr (&&) True (zipWith almostEqualSol x y)
  else False

sol1 :: PseudoSol
sol1 = (1.10,
        fromList [fromJust (mkStampSet 1.82 0),
                  fromJust (mkStampSet 1.05 1),
                  fromJust (mkStampSet 0.05 1)],
        fromList [fromJust (mkStampSet 1.82 1),
                  fromJust (mkStampSet 1.05 2),
                  fromJust (mkStampSet 0.05 3)])

boundary1 :: [PseudoSol]
boundary1 = [(1.10,
              fromList [fromJust (mkStampSet 1.82 0),
                        fromJust (mkStampSet 1.05 1),
                        fromJust (mkStampSet 0.05 1)],
              fromList [fromJust (mkStampSet 1.82 1),
                        fromJust (mkStampSet 1.05 2),
                        fromJust (mkStampSet 0.05 3)]),
             (1.82,
              fromList [fromJust (mkStampSet 1.82 1)],
              fromList [fromJust (mkStampSet 1.82 0),
                        fromJust (mkStampSet 1.05 3),
                        fromJust (mkStampSet 0.05 4)]),
             (2.1,
              fromList [fromJust (mkStampSet 1.82 0),
                        fromJust (mkStampSet 1.05 2)],
              fromList [fromJust (mkStampSet 1.82 1),
                        fromJust (mkStampSet 1.05 1),
                        fromJust (mkStampSet 0.05 4)])]

propTotalCost :: Double -> Seq StampSet -> Bool
propTotalCost x inventory = case optimum x' inventory of
  Left _ -> True -- FIXME: find a way to dismiss these results.
  Right sol -> solutionCost sol >= x'
  where
    x' = 0.01 + abs x

propResultingInventory :: Double -> Seq StampSet -> Bool
propResultingInventory x inventory = case optimum x' inventory of
  Left _ -> True -- FIXME: find a way to dismiss these results.
  Right sol -> foldr go True (resultingInventory sol)
  where
    x' = 0.01 + abs x
    go :: StampSet -> Bool -> Bool
    go s b = b && (quantity s >= 0)

spec :: Spec
spec = do
  describe "boundary" $ do
    it "find the boundary of the set of admissible solutions for the postage cost problem" $
      ((boundary 1.06 sq1) `eitherEquals` (Right boundary1)) `shouldBe` True

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      ((boundary 6 sq1) `eitherEquals` (Left "The problem is infeasible!")) `shouldBe` True

    it "should fail if the cost is zero" $
      ((boundary 0 sq1) `eitherEquals` (Left "The total cost should be a positive float!")) `shouldBe` True

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((boundary (0.001 + abs x) Empty) `eitherEquals` (Left "The problem is infeasible!"))

    it "should always fail if the total cost < 0" $ property $
      \ x -> ((boundary (-(abs x)) Empty) `eitherEquals` (Left "The total cost should be a positive float!"))

  describe "optimum" $ do
    it "choose the least costly combination of stamps for a letter" $
      ((optimum 1.06 sq1) `eitherEqual` (Right sol1)) `shouldBe` True

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      ((optimum 6 sq1) `eitherEqual` (Left "The problem is infeasible!")) `shouldBe` True

    it "should fail if the cost is zero" $
      ((optimum 0 sq1) `eitherEqual` (Left "The total cost should be a positive float!")) `shouldBe` True

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((optimum (0.001 + abs x) Empty) `eitherEqual` (Left "The problem is infeasible!"))

    it "should always fail if the total cost < 0" $ property $
      \ x -> ((optimum (-(abs x)) Empty) `eitherEqual` (Left "The total cost should be a positive float!"))

    -- TODO propriété : s'il y a une solution, le coût total est toujours supérieur ou égal au coût de la lettre
    it "should always give a solution (if it exists) with a cost greater or equal to the cost of the letter" $ property $
      propTotalCost

    -- TODO propriété : s'il y a une solution, l'inventaire résultant contient toujours un nombre positif ou nul de timbre
    it "should never return an inventory with a negative number of stamps" $ property $
      propResultingInventory