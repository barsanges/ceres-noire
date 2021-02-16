{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : StampSetSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module StampSet.
-}

module StampSetSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe ( fromJust )
import Data.Sequence ( Seq, fromList )

import StampSet

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (fromJust (mkStampSet (1e-9 + abs p) (abs q)))

s1 :: StampSet
s1 = fromJust (mkStampSet 1.1 2)

s2 :: StampSet
s2 = fromJust (mkStampSet 2.2 1)

s3 :: StampSet
s3 = fromJust (mkStampSet 0.57 21)

sq1 :: Seq StampSet
sq1 = fromList [s1, s2]

eitherEqual :: Either String (Seq StampSet) -> Either String (Seq StampSet) -> Bool
eitherEqual (Left x) (Left y) = x == y
eitherEqual (Right _) (Left _) = False
eitherEqual (Left _) (Right _) = False
eitherEqual (Right x) (Right y) = x `almostEqualSeq` y

spec :: Spec
spec = do
  describe "almostEqual" $ do
    it "compares two sets of stamps (1)" $
      (s1 `almostEqual` s1) `shouldBe` True

    it "compares two sets of stamps (2)" $
      (s1 `almostEqual` s2) `shouldBe` False

    it "always return True when comparing a stamp set with itself" $ property $
      \ x -> x `almostEqual` x

  describe "almostEqualSeq" $ do
    it "compares two sequences of stamp sets (1)" $
      (sq1 `almostEqualSeq` sq1)  `shouldBe` True

    it "compares two sequences of stamp sets (2)" $
      (sq1 `almostEqualSeq` (fromList [s1, s3]))  `shouldBe` False

    it "compares two sequences of stamp sets (3)" $
      (sq1 `almostEqualSeq` (fromList [s1]))  `shouldBe` False

    it "always return True when comparing a sequence of stamp set with itself" $ property $
      \ x -> x `almostEqualSeq` x

  describe "fromByteString" $ do
    it "converts a byte string to a sequence of stamp sets" $
      ((fromByteString "price,quantity\r\n1.1,2\r\n2.2,1\r\n") `eitherEqual` (Right sq1)) `shouldBe` True

    it "accepts a header with more columns than requested" $
      ((fromByteString "price,quantity,foo\r\n1.1,2,abc\r\n2.2,1,xyz\r\n") `eitherEqual` (Right sq1)) `shouldBe` True

    it "fails if the header does not contain 'price' and 'quantity'" $
      ((fromByteString "foo,bar\r\n1.1,27\r\n") `eitherEqual` (Left "parse error (Failed reading: conversion error: no field named \"price\") at \"\\r\\n\"")) `shouldBe` True

    it "fails if fields are missing" $
      ((fromByteString "price,quantity\r\n1.1,\r\n2.2,1\r\n") `eitherEqual` (Left "parse error (Failed reading: conversion error: expected Int, got \"\" (not enough input)) at \"\\r\\n2.2,1\\r\\n\"")) `shouldBe` True

    it "fails if fields have the wrong type" $
      ((fromByteString "price,quantity\r\n1.1,abc\r\n2.2,1\r\n") `eitherEqual` (Left "parse error (Failed reading: conversion error: expected Int, got \"abc\" (Failed reading: takeWhile1)) at \"\\r\\n2.2,1\\r\\n\"")) `shouldBe` True

    it "fails if the string contains no data" $
      ((fromByteString "price,quantity") `eitherEqual` (Left "parse error (not enough input) at \"\"")) `shouldBe` True

  describe "toByteString" $ do
    it "converts a sequence of stamp sets to a byte string" $
      toByteString sq1 `shouldBe` "price,quantity\r\n1.1,2\r\n2.2,1\r\n"