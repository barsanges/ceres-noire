{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : StampsSpec
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Test the module Stamps.
-}

module StampsSpec ( spec ) where

import Test.Hspec
import Test.QuickCheck

import Data.Maybe ( fromJust )

import Stamps

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (fromJust (mkStampSet 0 (1 + abs p) (abs q)))

s1 :: StampSet
s1 = fromJust (mkStampSet 2 1.10 2)

s2 :: StampSet
s2 = fromJust (mkStampSet 2 2.20 1)

sq1 :: [StampSet]
sq1 = [s1, s2]

sq2 :: [StampSet]
sq2 = [ fromJust (mkStampSet 2 1.00 2)
      , fromJust (mkStampSet 2 3.00 1)
      ]

sq3 :: [StampSet]
sq3 = [ fromJust (mkStampSet 2 1.00 1)
      , fromJust (mkStampSet 2 2.00 2)
      ]

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

  describe "Collection" $ do
    it "is an instance of Eq (1)" $
      sq1 `shouldBe` sq1

    it "is an instance of Eq (2)" $
      sq1 `shouldNotBe` sq2

    it "is an instance of Ord (1)" $
      (compare sq1 sq1) `shouldBe` EQ

    it "is an instance of Ord (2)" $
      (compare sq1 sq2) `shouldNotBe` EQ

    it "is an instance of Ord (3)" $
      (compare sq2 sq1) `shouldNotBe` EQ

    it "is an instance of Ord (4)" $
      (compare sq2 sq3) `shouldNotBe` EQ

  describe "fromByteString, with semi-colon" $ do
    it "converts a byte string to a sequence of stamp sets" $
      (fromByteString False 2 "price;quantity\r\n1.1;2\r\n2.2;1\r\n") `shouldBe` (Right sq1)

    it "accepts a header with more columns than requested" $
      (fromByteString False 2 "price;quantity;foo\r\n1.1;2;abc\r\n2.2;1;xyz\r\n") `shouldBe` (Right sq1)

    it "fails if the header does not contain 'price' and 'quantity'" $
      (fromByteString False 2 "foo;bar\r\n1.1;27\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: no field named \"price\") at \"\\r\\n\"")

    it "fails if fields are missing" $
      (fromByteString False 2 "price;quantity\r\n1.1;\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"\" (not enough input)) at \"\\r\\n2.2;1\\r\\n\"")

    it "fails if fields have the wrong type" $
      (fromByteString False 2 "price;quantity\r\n1.1;abc\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"abc\" (Failed reading: takeWhile1)) at \"\\r\\n2.2;1\\r\\n\"")

    it "fails if the string contains no data" $
      (fromByteString False 2 "price;quantity") `shouldBe` (Left "parse error (not enough input) at \"\"")

    it "fails if the string contains negative values" $
      (fromByteString False 2 "price;quantity\r\n1.1;-2\r\n2.2;1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: Invalid data!) at \"\\r\\n2.2;1\\r\\n\"")

  describe "fromByteString, with comma" $ do
    it "converts a byte string to a sequence of stamp sets" $
      (fromByteString True 2 "price,quantity\r\n1.1,2\r\n2.2,1\r\n") `shouldBe` (Right sq1)

    it "accepts a header with more columns than requested" $
      (fromByteString True 2 "price,quantity,foo\r\n1.1,2,abc\r\n2.2,1,xyz\r\n") `shouldBe` (Right sq1)

    it "fails if the header does not contain 'price' and 'quantity'" $
      (fromByteString True 2 "foo,bar\r\n1.1,27\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: no field named \"price\") at \"\\r\\n\"")

    it "fails if fields are missing" $
      (fromByteString True 2 "price,quantity\r\n1.1,\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"\" (not enough input)) at \"\\r\\n2.2,1\\r\\n\"")

    it "fails if fields have the wrong type" $
      (fromByteString True 2 "price,quantity\r\n1.1,abc\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: expected Int, got \"abc\" (Failed reading: takeWhile1)) at \"\\r\\n2.2,1\\r\\n\"")

    it "fails if the string contains no data" $
      (fromByteString True 2 "price,quantity") `shouldBe` (Left "parse error (not enough input) at \"\"")

    it "fails if the string contains negative values" $
      (fromByteString True 2 "price,quantity\r\n1.1,-2\r\n2.2,1\r\n") `shouldBe` (Left "parse error (Failed reading: conversion error: Invalid data!) at \"\\r\\n2.2,1\\r\\n\"")
