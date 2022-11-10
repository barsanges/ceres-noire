{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

sq1 :: Seq StampSet
sq1 = fromList [s1, s2]

stampSetEq :: StampSet -> Bool
stampSetEq x = x == x

spec :: Spec
spec = do
  describe "StampSet" $ do
    it "is an instance of Eq (1)" $
      s1 `shouldBe` s1

    it "is an instance of Eq (2)" $
      s1 `shouldNotBe` s2

    it "a stamp set is always equal to itself" $ property $
      stampSetEq

  describe "fromByteString, with semi-colon" $ do
    it "converts a byte string to a sequence of stamp sets" $
      (fromByteString False "price;quantity\r\n1.1;2\r\n2.2;1\r\n") `shouldBe` (Right sq1)

    it "accepts a header with more columns than requested" $
      (fromByteString False "price;quantity;foo\r\n1.1;2;abc\r\n2.2;1;xyz\r\n") `shouldBe` (Right sq1)

    it "fails if the header does not contain 'price' and 'quantity'" $
      (fromByteString False "foo;bar\r\n1.1;27\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: no field named \"price\") at \"\\r\\n\"")

    it "fails if fields are missing" $
      (fromByteString False "price;quantity\r\n1.1;\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"\" (not enough input)) at \"\\r\\n2.2;1\\r\\n\"")

    it "fails if fields have the wrong type" $
      (fromByteString False "price;quantity\r\n1.1;abc\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"abc\" (Failed reading: takeWhile1)) at \"\\r\\n2.2;1\\r\\n\"")

    it "fails if the string contains no data" $
      (fromByteString False "price;quantity") `shouldBe` (Left "parse error (not enough input) at \"\"")

    it "fails if the string contains negative values" $
      (fromByteString False "price;quantity\r\n1.1;-2\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: Invalid data!) at \"\\r\\n2.2;1\\r\\n\"")

  describe "fromByteString, with comma" $ do
    it "converts a byte string to a sequence of stamp sets" $
      (fromByteString True "price,quantity\r\n1.1,2\r\n2.2,1\r\n") `shouldBe` (Right sq1)

    it "accepts a header with more columns than requested" $
      (fromByteString True "price,quantity,foo\r\n1.1,2,abc\r\n2.2,1,xyz\r\n") `shouldBe` (Right sq1)

    it "fails if the header does not contain 'price' and 'quantity'" $
      (fromByteString True "foo,bar\r\n1.1,27\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: no field named \"price\") at \"\\r\\n\"")

    it "fails if fields are missing" $
      (fromByteString True "price,quantity\r\n1.1,\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"\" (not enough input)) at \"\\r\\n2.2,1\\r\\n\"")

    it "fails if fields have the wrong type" $
      (fromByteString True "price,quantity\r\n1.1,abc\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"abc\" (Failed reading: takeWhile1)) at \"\\r\\n2.2,1\\r\\n\"")

    it "fails if the string contains no data" $
      (fromByteString True "price,quantity") `shouldBe` (Left "parse error (not enough input) at \"\"")

    it "fails if the string contains negative values" $
      (fromByteString True "price,quantity\r\n1.1,-2\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: Invalid data!) at \"\\r\\n2.2,1\\r\\n\"")
