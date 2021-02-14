{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : StampSet
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Stamps and sequences of stamps for the postage cost problem.
-}

module StampSet (
  StampSet,
  mkStamp,
  price,
  quantity,
  split,
  readInventory,
  writeInventory
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Foldable ( toList )
import Data.Sequence ( Seq, fromList )
import qualified Data.Vector as V

-- | A set of similar stamps is defined by the unitary price of the price, and
-- the quantity of stamps in the set.
data StampSet = StampSet Float Int

instance Csv.FromNamedRecord StampSet where
  parseNamedRecord r = do
    p <- Csv.lookup r "price"
    q <- Csv.lookup r "quantity"
    let s = mkStamp p q
    case s of
      Just stampSet -> return stampSet
      Nothing -> fail "Invalid data!" -- FIXME: improve the error message.

instance Csv.ToNamedRecord StampSet where
  toNamedRecord (StampSet p q) = Csv.namedRecord [
    Csv.namedField "price" p,
    Csv.namedField "quantity" q]

-- | Create a set of stamp.
mkStamp :: Float -> Int -> Maybe StampSet
mkStamp p q = if (p > 0) && (q > 0)
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

-- | Read a sequence of stamp sets from a CSV-like bytestring.
readInventory :: BL.ByteString -> Either String (Seq StampSet)
readInventory bs = case Csv.decodeByName bs of
  Left msg -> Left msg
  Right (_, x) -> Right ((fromList . V.toList) x)

-- | Turn a sequence of stamp sets to a CSV-like bytestring.
writeInventory :: (Seq StampSet) -> BL.ByteString
writeInventory stamps = Csv.encodeByName header (toList stamps)
  where
    header = V.fromList ["price", "quantity"]