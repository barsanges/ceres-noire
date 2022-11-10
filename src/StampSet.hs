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
  setValue,
  totalValue,
  totalQuantity,
  mkRange,
  split,
  simplify,
  fromByteString,
  readInventoryFile,
  readInventoryString,
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char ( ord )
import qualified Data.Csv as Csv
import Data.Sequence ( Seq(..), (<|), fromList )
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Vector as V

-- | A set of similar stamps is defined by the unitary price of the stamp, and
-- the quantity of stamps in the set.
data StampSet = StampSet Double Int
  deriving Show

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

instance Eq StampSet where
  x == y = (abs (price x - price y) < eps)
           && (abs (quantity x - quantity y) == 0)

instance Ord StampSet where
  compare x y = if abs (price x - price y) < eps
                then compare (quantity x) (quantity y)
                else compare (price x) (price y)

-- | Threshold for float equality.
eps :: Double
eps = 1e-12

-- | Create a set of stamp.
mkStampSet :: Double -> Int -> Maybe StampSet
mkStampSet p q = if (p > 0) && (q >= 0)
  then Just (StampSet p q)
  else Nothing

-- | Get the unitary price of a stamp.
price :: StampSet -> Double
price (StampSet p _) = p

-- | Get the available number of stamps in a set of stamps.
quantity :: StampSet -> Int
quantity (StampSet _ q) = q

-- | Get the total value of a set of stamps.
setValue :: StampSet -> Double
setValue (StampSet p q) = p * (fromIntegral q)

-- | Get the total value of a sequence of stamps sets.
totalValue :: Seq StampSet -> Double
totalValue = (sum . (fmap setValue))

-- | Get the total number of stamps in a sequence of stamps sets.
totalQuantity :: Seq StampSet -> Int
totalQuantity = (sum . (fmap quantity))

-- | Create a sequence of sets of length `1 + quantity s`. The first set
-- contains zero stamp, the second one, etc, and the last one `quantity s`.
mkRange :: StampSet -> Seq StampSet
mkRange (StampSet p q) = S.fromList [ StampSet p i | i <- [0..q] ]

-- | Split a set of stamps in two different parts, one with 'n' pieces and the
-- other with the rest. If the operation is not feasible ('n < 0' or 'n' is
-- larger than the number of stamps available), the first set of the pair is
-- empty.
split :: StampSet -> Int -> (StampSet, StampSet)
split (StampSet p q) n = if (n <= q) && (n >= 0)
  then (StampSet p n, StampSet p (q - n))
  else (StampSet p 0, StampSet p q)

-- | Simplify a sequence of stamps sets by merging the sets of stamps which
-- have the same price.
simplify :: Seq StampSet -> Seq StampSet
simplify Empty = Empty
simplify ((StampSet p q) :<| xs) = if q' > 0
  then (StampSet p q') <| (simplify xs')
  else simplify xs'
  where
    (ys, xs') = S.partition (\ y -> abs (price y - p) < 1e-12) xs
    q' = q + totalQuantity ys

-- | Read a sequence of stamp sets from a CSV-like bytestring.
fromByteString :: Bool -> BL.ByteString -> Either String (Seq StampSet)
fromByteString comma bs = case Csv.decodeByNameWith myOptions bs of
  Left msg -> Left msg
  Right (_, x) -> Right ((fromList . V.toList) x)
  where
    myOptions = if comma
      then Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
      else Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }

-- | Read a sequence of stamp sets from a CSV file.
readInventoryFile :: Bool -> String -> IO (Either String (Seq StampSet))
readInventoryFile comma fname = do
  csvData <- BL.readFile fname
  return (fromByteString comma csvData)

-- | Read a sequence of stamp sets from a CSV-like string.
readInventoryString :: Bool -> String -> Either String (Seq StampSet)
readInventoryString comma csvData = fromByteString comma (encodeUtf8 . T.pack $ csvData)
