{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : StampSet
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Stamps and sequences of stamps for the postage cost problem.
-}

module StampSet (
  StampSet,
  mkStampSet,
  price,
  quantity,
  split,
  almostEqual,
  almostEqualSeq,
  fromByteString,
  readInventory,
  toByteString,
  writeInventory
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Foldable ( toList )
import Data.Sequence ( Seq, fromList )
import qualified Data.Sequence as S
import qualified Data.Vector as V

-- | A set of similar stamps is defined by the unitary price of the price, and
-- the quantity of stamps in the set.
data StampSet = StampSet Float Int

instance Csv.FromNamedRecord StampSet where
  parseNamedRecord r = do
    p <- Csv.lookup r "price"
    q <- Csv.lookup r "quantity"
    let s = mkStampSet p q
    case s of
      Just stampSet -> return stampSet
      Nothing -> fail "Invalid data!" -- FIXME: improve the error message.

instance Csv.ToNamedRecord StampSet where
  toNamedRecord (StampSet p q) = Csv.namedRecord [
    Csv.namedField "price" p,
    Csv.namedField "quantity" q]

-- | Create a set of stamp.
mkStampSet :: Float -> Int -> Maybe StampSet
mkStampSet p q = if (p > 0) && (q > 0)
  then Just (StampSet p q)
  else Nothing

-- | Get the unitary price of a stamp.
price :: StampSet -> Float
price (StampSet p _) = p

-- | Get the available number of stamps in a set of stamps.
quantity :: StampSet -> Int
quantity (StampSet _ q) = q

-- | Split a set of stamps in two different parts, one with 'n' pieces and the
-- other with the rest. If the operation is not feasible ('n < 0' of 'n' is
-- larger than the number of stamps available), the first set of the pair is
-- empty.
split :: StampSet -> Int -> (StampSet, StampSet)
split (StampSet p q) n = if (n <= q) && (n >= 0)
  then (StampSet p n, StampSet p (q - n))
  else (StampSet p 0, StampSet p q)

-- | Test if two stamp sets are equal (float precision: 1e-12).
almostEqual :: StampSet -> StampSet -> Bool
almostEqual (StampSet p q) (StampSet p' q') = (abs (p - p') < 1e-12) && (q == q')

-- | Test if two sequences of stamp sets are equal (float precision: 1e-12).
almostEqualSeq :: Seq StampSet -> Seq StampSet -> Bool
almostEqualSeq x y = if (length x) == (length y)
  then foldr (&&) True (S.zipWith almostEqual x y)
  else False

-- | Read a sequence of stamp sets from a CSV-like bytestring.
fromByteString :: BL.ByteString -> Either String (Seq StampSet)
fromByteString bs = case Csv.decodeByName bs of
  Left msg -> Left msg
  Right (_, x) -> Right ((fromList . V.toList) x)

-- | Read a sequence of stamp sets from a CSV file.
readInventory :: String -> IO (Either String (Seq StampSet))
readInventory fname = do
  csvData <- BL.readFile fname
  return (fromByteString csvData)

-- | Turn a sequence of stamp sets to a CSV-like bytestring.
toByteString :: (Seq StampSet) -> BL.ByteString
toByteString stamps = Csv.encodeByName header (toList stamps)
  where
    header = V.fromList ["price", "quantity"]

-- | Read a sequence of stamp sets to a CSV file.
writeInventory :: String -> (Seq StampSet) -> IO ()
writeInventory fname stamps = BL.writeFile fname (toByteString stamps)