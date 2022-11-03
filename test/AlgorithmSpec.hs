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

import Data.Either ( isRight )
import Data.Maybe ( fromJust )
import Data.Sequence ( Seq(..), fromList )
import qualified Data.Sequence as Seq

import Algorithm
import StampSet

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (fromJust (mkStampSet (1e-9 + abs p) (abs q)))

sq1 :: Seq StampSet
sq1 = fromList [ fromJust (mkStampSet 1.08 3)
               , fromJust (mkStampSet 1.43 2)
               , fromJust (mkStampSet 2.86 1)
               ]

sq2 :: Seq StampSet
sq2 = fromList [ fromJust (mkStampSet 1.28 6)
               , fromJust (mkStampSet 2.32 2)
               , fromJust (mkStampSet 2.56 2)
               ]

sol1 :: Seq (Seq StampSet)
sol1 = fromList [ fromList [fromJust (mkStampSet 2.86 1)]
                , fromList [fromJust (mkStampSet 1.43 2)]
                ]

sol2 :: Seq (Seq StampSet)
sol2 = fromList [ fromList [fromJust (mkStampSet 2.32 1)] ]

sol3 :: Seq (Seq StampSet)
sol3 = fromList [ fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 1)]
                , fromList [fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 3)]
                , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 1)]
                , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 3)]
                , fromList [fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 5)]
                , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 1.28 3)]
                , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 1.28 5)]
                , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 2.32 2)]
                , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 2)]
                , fromList [fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 4)]
                , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 2)]
                , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 4)]
                , fromList [fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 6)]
                ]

probGen :: Int -> Gen (Double, Double, Seq StampSet)
probGen n = do
  x <- arbitrary
  y <- arbitrary
  z <- resize n (fmap fromList (listOf arbitrary))
  return (x, y, z)

propTotalCost (x, y, inventory) = ok ==> cover 99 ok "non-trivial" prop
  where
    x' = 0.01 + abs x
    y' = x' + 10 * (abs y)
    res = withinRange x' y' inventory
    ok = case res of
      Left _ -> False
      Right zs -> length zs > 0
    prop = case res of
      Left _ -> False -- Never happens in practice.
      Right zs -> all (\ z -> (totalValue z) >= x' && (totalValue z) <= y') zs

multiple1 :: Seq (Seq StampSet)
multiple1 = fromList [ fromList [fromJust (mkStampSet 1.28 1)]
                     , fromList [fromJust (mkStampSet 1.28 2)]
                     ]

simplified1 :: Seq (Seq StampSet)
simplified1 = fromList [ fromList [fromJust (mkStampSet 1.28 1)]
                       ]

multiple2 :: Seq (Seq StampSet)
multiple2 = fromList [ fromList [fromJust (mkStampSet 1.28 1)]
                     , fromList [fromJust (mkStampSet 1.28 1), fromJust (mkStampSet 0.10 1)]
                     ]

simplified2 :: Seq (Seq StampSet)
simplified2 = fromList [ fromList [fromJust (mkStampSet 1.28 1)]
                       ]

multiple3 :: Seq (Seq StampSet)
multiple3 = fromList [ fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 1.28 3)]
                     , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 1.28 1)]
                     ]

simplified3 :: Seq (Seq StampSet)
simplified3 = fromList [ fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 1.28 3)]
                       , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 1.28 1)]
                       ]

simplified4 :: Seq (Seq StampSet)
simplified4 = fromList [ fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 1)]
                       , fromList [fromJust (mkStampSet 2.32 2), fromJust (mkStampSet 1.28 3)]
                       , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 1)]
                       , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 3)]
                       , fromList [fromJust (mkStampSet 2.32 1), fromJust (mkStampSet 1.28 5)]
                       , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 1.28 3)]
                       , fromList [fromJust (mkStampSet 2.56 1), fromJust (mkStampSet 1.28 5)]
                       , fromList [fromJust (mkStampSet 2.56 2), fromJust (mkStampSet 2.32 2)]
                       ]

spec :: Spec
spec = do
  describe "withinRange" $ do
    it "finds the sets of stamps whose total value lies within the given range (1)" $
      (withinRange 2.80 2.90 sq1) `shouldBe` (Right sol1)

    it "finds the sets of stamps whose total value lies within the given range (2)" $
      (withinRange 2.32 2.33 sq2) `shouldBe` (Right sol2)

    it "finds the sets of stamps whose total value lies within the given range (3)" $
      (withinRange 8 10 sq2) `shouldBe` (Right sol3)

    it "should fail if the cost of the letter is bigger than the total value of the inventory" $
      (withinRange 11 11 sq1) `shouldBe` (Left "The problem is infeasible!")

    it "should always fail if the inventory is empty" $ property $
      \ x -> ((withinRange (abs x) (0.1 + abs x) Empty) `shouldBe` (Left "The problem is infeasible!"))

    it "should always fail if the minimum value is negative" $ property $
      \ x -> ((withinRange (-(abs x) - 0.1) (abs x) Empty) `shouldBe` (Left "The minimum value should be a positive float!"))

    it "should always fail if the maximum value is lower than the minimum value" $ property $
      \ x -> ((withinRange (abs x + 0.1) ((abs x + 0.1) * 0.5) Empty) `shouldBe` (Left "The maximum value should be greater than the minimum value!"))

    it "should always give solutions (if they exists) with a cost greater or equal to the cost of the letter" $ property $
      forAll (probGen 5) propTotalCost

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
      (dropSupersets Empty) `shouldBe` Empty

    it "should never return a sequence longer than the original one" $ property $
      \ xs -> ((length $ dropSupersets xs) <= (length xs))

    it "should leave singletons untouched" $ property $
      \ x -> ((dropSupersets $ Seq.singleton $ Seq.singleton x) == (Seq.singleton $ Seq.singleton x))
