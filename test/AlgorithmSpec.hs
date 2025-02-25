{-# OPTIONS_GHC -fno-warn-orphans #-}
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

import Algorithm
import Stamps

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (fromJust (mkStampSet 0 (1 + abs p) (abs q)))

-- | xs `eitherShouldMatch` ys sets the expectation that xs has the
-- same elements that ys has, possibly in another order.
eitherShouldMatch :: (HasCallStack, Show a, Eq a)
                  => Either String [a]
                  -> Either String [a]
                  -> Expectation
eitherShouldMatch (Left x) (Left y) = x `shouldBe` y
eitherShouldMatch (Left _) (Right _) = expectationFailure "cannot compare Left and Right"
eitherShouldMatch (Right _) (Left _) = expectationFailure "cannot compare Right and Left"
eitherShouldMatch (Right x) (Right y) = x `shouldMatchList` y

sq1 :: Collection
sq1 = [ fromJust (mkStampSet 2 1.08 3)
      , fromJust (mkStampSet 2 1.43 2)
      , fromJust (mkStampSet 2 2.86 1)
      ]

sq2 :: Collection
sq2 = [ fromJust (mkStampSet 2 2.56 2)
      , fromJust (mkStampSet 2 2.32 2)
      , fromJust (mkStampSet 2 1.28 6)
      ]

sol1 :: [Collection]
sol1 = [ [fromJust (mkStampSet 2 2.86 1)]
       , [fromJust (mkStampSet 2 1.43 2)]
       ]

sol2 :: [Collection]
sol2 = [ [fromJust (mkStampSet 2 2.32 1)] ]

sol3 :: [Collection]
sol3 = [ [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 1)]
       , [fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 3)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 1)]
       , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 3)]
       , [fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 5)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 1.28 3)]
       , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 1.28 5)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 2)]
       , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 2)]
       , [fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 4)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 2)]
       , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 4)]
       , [fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 6)]
       ]

sol4 :: [Collection]
sol4 = [ [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 1)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 1)]
       , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 2)]
       ]

probGen :: Int -> Gen (Int, Double, Double, Collection)
probGen n = do
  q <- arbitrary
  x <- arbitrary
  y <- arbitrary
  z <- resize n (listOf arbitrary)
  return (q, x, y, z)

propTotalCost :: (Int, Double, Double, Collection) -> Property
propTotalCost (n, x, y, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    eps = 1e-9
    x' = abs x
    y' = x' + (abs y)
    n' = 1 + (abs n)
    res = withinRange eps (Just n') x' y' inventory
    ok = case res of
      Left _ -> False
      Right zs -> length zs > 0
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right zs -> all (\ z -> (totalValue z) >= x' - eps && (totalValue z) <= y' + eps) zs

propTotalQuantity :: (Int, Double, Double, Collection) -> Property
propTotalQuantity (n, x, y, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    eps = 1e-9
    x' = abs x
    y' = x' + (abs y)
    n' = 1 + (abs n)
    res = withinRange eps (Just n') x' y' inventory
    ok = case res of
      Left _ -> False
      Right zs -> length zs > 0
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right zs -> all (\ z -> (totalQuantity z) <= n') zs

multiple1 :: [Collection]
multiple1 = [ [fromJust (mkStampSet 2 1.28 1)]
            , [fromJust (mkStampSet 2 1.28 2)]
            ]

simplified1 :: [Collection]
simplified1 = [ [fromJust (mkStampSet 2 1.28 1)] ]

multiple2 :: [Collection]
multiple2 = [ [fromJust (mkStampSet 2 1.28 1)]
            , [fromJust (mkStampSet 2 1.28 1), fromJust (mkStampSet 2 0.10 1)]
            ]

simplified2 :: [Collection]
simplified2 = [ [fromJust (mkStampSet 2 1.28 1)] ]

multiple3 :: [Collection]
multiple3 = [ [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 1.28 3)]
            , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 1.28 1)]
            ]

simplified3 :: [Collection]
simplified3 = [ [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 1.28 3)]
              , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 1.28 1)]
              ]

simplified4 :: [Collection]
simplified4 = [ [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 1)]
              , [fromJust (mkStampSet 2 2.32 2), fromJust (mkStampSet 2 1.28 3)]
              , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 1)]
              , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 3)]
              , [fromJust (mkStampSet 2 2.32 1), fromJust (mkStampSet 2 1.28 5)]
              , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 1.28 3)]
              , [fromJust (mkStampSet 2 2.56 1), fromJust (mkStampSet 2 1.28 5)]
              , [fromJust (mkStampSet 2 2.56 2), fromJust (mkStampSet 2 2.32 2)]
              ]

spec :: Spec
spec = do
  describe "withinRange" $ do
    it "finds the sets of stamps whose total value lies within the given range (1)" $
      (withinRange 1e-9 Nothing 2.80 2.90 sq1) `eitherShouldMatch` (Right sol1)

    it "finds the sets of stamps whose total value lies within the given range (2)" $
      (withinRange 1e-9 Nothing 2.32 2.33 sq2) `eitherShouldMatch` (Right sol2)

    it "finds the sets of stamps whose total value lies within the given range (3)" $
      (withinRange 1e-9 Nothing 8.00 10.00 sq2) `eitherShouldMatch` (Right sol3)

    it "finds the sets of stamps whose total value lies within the given range (4)" $
      (withinRange 1e-9 (Just 4) 8.00 10.00 sq2) `eitherShouldMatch` (Right sol4)

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      (withinRange 1e-9 Nothing 11.00 11.00 sq1) `eitherShouldMatch` (Left "The problem is infeasible!")

    it "should fail if the maximal number of stamps is too low" $
      (withinRange 1e-9 (Just 3) 8.00 10.00 sq2) `eitherShouldMatch` (Left "The problem is infeasible!")

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((withinRange 1e-9 Nothing (abs x) (1 + abs x) empty) `eitherShouldMatch` (Left "The problem is infeasible!"))

    it "should always fail if the minimum value is negative" $ property $
      \ x -> ((withinRange 1e-9 Nothing (-(abs x) - 1) (abs x) empty) `eitherShouldMatch` (Left "The minimum value should be a positive float!"))

    it "should always fail if the maximum value is lower than the minimum value" $ property $
      \ x -> ((withinRange 1e-9 Nothing ((abs x) + 10) ((abs x) + 5) empty) `eitherShouldMatch` (Left "The maximum value should be greater than the minimum value!"))

    it "should always fail if the maximal number of stamp is lower or equal to 0" $ property $
      \ n x -> ((withinRange 1e-9 (Just (-(abs n))) (abs x) (abs (2 * x)) empty) `eitherShouldMatch` (Left "The maximal number of stamps should be strictly positive!"))

    it "should always give solutions (if they exist) with a cost greater or equal to the cost of the letter" $ property $
      forAll (probGen 5) propTotalCost

    it "should always give solutions (if they exist) with a number of stamps lower or equal to the maximal number of stamps allowed" $ property $
      forAll (probGen 5) propTotalQuantity

  describe "dropSupersets" $ do
    it "filter the sets that are supersets of other sets in the sequence (1)" $
      (dropSupersets multiple1) `shouldBe` simplified1

    it "filter the sets that are supersets of other sets in the sequence (2)" $
      (dropSupersets multiple2) `shouldBe` simplified2

    it "filter the sets that are supersets of other sets in the sequence (3)" $
      (dropSupersets multiple3) `shouldBe` simplified3

    it "filter the sets that are supersets of other sets in the sequence (4)" $
      (dropSupersets sol3) `shouldBe` simplified4

    it "filter the sets that are supersets of other sets in the sequence (5)" $
      (dropSupersets []) `shouldBe` []

    it "should never return a sequence longer than the original one" $ property $
      \ xs -> ((length $ dropSupersets xs) <= (length xs))

    it "should leave singletons untouched" $ property $
      \ x -> ((dropSupersets $ [[x]]) == [[x]])
