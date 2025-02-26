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

import Stamps

instance Eq StampSet where
  x == y = (abs (price x - price y) < precision)
           && (abs (quantity x - quantity y) == 0)

instance Arbitrary StampSet where
  arbitrary = do
    p <- arbitrary
    q <- arbitrary
    return (St { price = (1 + abs p), quantity = (abs q) })

s1 :: StampSet
s1 = St { price = 1.10, quantity = 2 }

s2 :: StampSet
s2 = St { price = 2.20, quantity = 1 }

sq1 :: [StampSet]
sq1 = [s2, s1]

spec :: Spec
spec = do
  describe "fromByteString, with semi-colon" $ do
    it "converts a byte string to a sequence of stamp sets" $
      (fromByteString False "price;quantity\r\n1.1;2\r\n2.2;1\r\n") `shouldBe` (Right sq1)

    it "merges stamp sets with the same price" $
      (fromByteString False "price;quantity\r\n1.1;1\r\n2.2;1\r\n1.1;1\r\n") `shouldBe` (Right sq1)

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
