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

import Algorithm
import Stamps

instance Eq StampSet where
  x == y = (abs (price x - price y) < precision)
           && (abs (quantity x - quantity y) == 0)

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (St { price = (1 + abs p), quantity = (abs q) })

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

sq1 :: [StampSet]
sq1 = [ St { price = 1.08, quantity = 3 }
      , St { price = 1.43, quantity = 2 }
      , St { price = 2.86, quantity = 1 }
      ]

sq2 :: [StampSet]
sq2 = [ St { price = 2.56, quantity = 2 }
      , St { price = 2.32, quantity = 2 }
      , St { price = 1.28, quantity = 6 }
      ]

sol1 :: [[StampSet]]
sol1 = [ [St { price = 2.86, quantity = 1 }]
       , [St { price = 1.43, quantity = 2 }]
       ]

sol2 :: [[StampSet]]
sol2 = [ [St { price = 2.32, quantity = 1 }] ]

sol3 :: [[StampSet]]
sol3 = [ [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 1 }]
       , [St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 3 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 1 }]
       , [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 3 }]
       , [St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 5 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 1.28, quantity = 3 }]
       , [St { price = 2.56, quantity = 1 }, St { price = 1.28, quantity = 5 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 2 }]
       , [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 2 }]
       , [St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 4 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 2 }]
       , [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 4 }]
       , [St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 6 }]
       ]

sol4 :: [[StampSet]]
sol4 = [ [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 1 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 1 }]
       , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 2 }]
       ]

probGen :: Int -> Gen (Int, Double, Double, [StampSet])
probGen n = do
  q <- arbitrary
  x <- arbitrary
  y <- arbitrary
  z <- resize n (listOf arbitrary)
  return (q, x, y, z)

propTotalCost :: (Int, Double, Double, [StampSet]) -> Property
propTotalCost (n, x, y, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    x' = abs x
    y' = x' + (abs y)
    n' = 1 + (abs n)
    res = withinRange (Just n') x' y' inventory
    ok = case res of
      Left _ -> False
      Right zs -> length zs > 0
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right zs -> all (\ z -> (totalValue z) >= x' - precision && (totalValue z) <= y' + precision) zs

propTotalQuantity :: (Int, Double, Double, [StampSet]) -> Property
propTotalQuantity (n, x, y, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    x' = abs x
    y' = x' + (abs y)
    n' = 1 + (abs n)
    res = withinRange (Just n') x' y' inventory
    ok = case res of
      Left _ -> False
      Right zs -> length zs > 0
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right zs -> all (\ z -> (totalQuantity z) <= n') zs

multiple1 :: [[StampSet]]
multiple1 = [ [St { price = 1.28, quantity = 1 }]
            , [St { price = 1.28, quantity = 2 }]
            ]

simplified1 :: [[StampSet]]
simplified1 = [ [St { price = 1.28, quantity = 1 }] ]

multiple2 :: [[StampSet]]
multiple2 = [ [St { price = 1.28, quantity = 1 }]
            , [St { price = 1.28, quantity = 1 }, St { price = 0.10, quantity = 1 }]
            ]

simplified2 :: [[StampSet]]
simplified2 = [ [St { price = 1.28, quantity = 1 }] ]

multiple3 :: [[StampSet]]
multiple3 = [ [St { price = 2.56, quantity = 1 }, St { price = 1.28, quantity = 3 }]
            , [St { price = 2.56, quantity = 2 }, St { price = 1.28, quantity = 1 }]
            ]

simplified3 :: [[StampSet]]
simplified3 = [ [St { price = 2.56, quantity = 1 }, St { price = 1.28, quantity = 3 }]
              , [St { price = 2.56, quantity = 2 }, St { price = 1.28, quantity = 1 }]
              ]

simplified4 :: [[StampSet]]
simplified4 = [ [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 1 }]
              , [St { price = 2.32, quantity = 2 }, St { price = 1.28, quantity = 3 }]
              , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 1 }]
              , [St { price = 2.56, quantity = 1 }, St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 3 }]
              , [St { price = 2.32, quantity = 1 }, St { price = 1.28, quantity = 5 }]
              , [St { price = 2.56, quantity = 2 }, St { price = 1.28, quantity = 3 }]
              , [St { price = 2.56, quantity = 1 }, St { price = 1.28, quantity = 5 }]
              , [St { price = 2.56, quantity = 2 }, St { price = 2.32, quantity = 2 }]
              ]

spec :: Spec
spec = do
  describe "withinRange" $ do
    it "finds the sets of stamps whose total value lies within the given range (1)" $
      (withinRange Nothing 2.80 2.90 sq1) `eitherShouldMatch` (Right sol1)

    it "finds the sets of stamps whose total value lies within the given range (2)" $
      (withinRange Nothing 2.32 2.33 sq2) `eitherShouldMatch` (Right sol2)

    it "finds the sets of stamps whose total value lies within the given range (3)" $
      (withinRange Nothing 8.00 10.00 sq2) `eitherShouldMatch` (Right sol3)

    it "finds the sets of stamps whose total value lies within the given range (4)" $
      (withinRange (Just 4) 8.00 10.00 sq2) `eitherShouldMatch` (Right sol4)

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      (withinRange Nothing 11.00 11.00 sq1) `eitherShouldMatch` (Left "The problem is infeasible!")

    it "should fail if the maximal number of stamps is too low" $
      (withinRange (Just 3) 8.00 10.00 sq2) `eitherShouldMatch` (Left "The problem is infeasible!")

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((withinRange Nothing (abs x) (1 + abs x) []) `eitherShouldMatch` (Left "The problem is infeasible!"))

    it "should always fail if the minimum value is negative" $ property $
      \ x -> ((withinRange Nothing (-(abs x) - 1) (abs x) []) `eitherShouldMatch` (Left "The minimum value should be a positive float!"))

    it "should always fail if the maximum value is lower than the minimum value" $ property $
      \ x -> ((withinRange Nothing ((abs x) + 10) ((abs x) + 5) []) `eitherShouldMatch` (Left "The maximum value should be greater than the minimum value!"))

    it "should always fail if the maximal number of stamp is lower or equal to 0" $ property $
      \ n x -> ((withinRange (Just (-(abs n))) (abs x) (abs (2 * x)) []) `eitherShouldMatch` (Left "The maximal number of stamps should be strictly positive!"))

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
