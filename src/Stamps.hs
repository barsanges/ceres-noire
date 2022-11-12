{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Stamps
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Stamps and collections of stamps.
-}

module Stamps (
  StampSet,
  Collection,
  mkStampSet,
  price,
  quantity,
  setValue,
  totalValue,
  totalQuantity,
  mkRange,
  split,
  noStrictSubset,
  reprCollection,
  fromByteString,
  readInventoryFile,
  readInventoryString,
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char ( ord )
import qualified Data.Csv as Csv
import Data.Foldable ( toList )
import Data.List ( intercalate )
import Data.Sequence ( Seq(..), fromList )
import qualified Data.Sequence as S
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Vector as V
import Numeric ( showFFloat )

-- | A set of similar stamps is defined by the unitary price of the stamp, and
-- the quantity of stamps in the set.
data StampSet = StampSet Int Int
  deriving Show

instance Eq StampSet where
  x == y = (abs (price x - price y) == 0)
           && (abs (quantity x - quantity y) == 0)

instance Ord StampSet where
  compare x y = if abs (price x - price y) == 0
                then compare (quantity x) (quantity y)
                else compare (price x) (price y)

-- | A stamp set as it should be read from a CSV file.
data CsvStampSet = CsvStampSet Double Int

instance Csv.FromNamedRecord CsvStampSet where
  parseNamedRecord r = do
    p <- Csv.lookup r "price"
    q <- Csv.lookup r "quantity"
    if (p > 0) && (q >= 0)
      then return (CsvStampSet p q)
      else fail "Invalid data!" -- FIXME: improve the error message.

-- | A collection of stamps.
type Collection = Seq StampSet

-- | Create a set of stamp.
mkStampSet :: Int -> Int -> Maybe StampSet
mkStampSet p q = if (p > 0) && (q >= 0)
  then Just (StampSet p q)
  else Nothing

-- | Get the unitary price of a stamp.
price :: StampSet -> Int
price (StampSet p _) = p

-- | Get the available number of stamps in a set of stamps.
quantity :: StampSet -> Int
quantity (StampSet _ q) = q

-- | Get the total value of a set of stamps.
setValue :: StampSet -> Int
setValue (StampSet p q) = p * q

-- | Get the total value of a sequence of stamps sets.
totalValue :: Collection -> Int
totalValue = (sum . (fmap setValue))

-- | Get the total number of stamps in a sequence of stamps sets.
totalQuantity :: Collection -> Int
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

-- | `noSubset ss s` returns `True` if `s` has no strict subset in `ss`.
noStrictSubset :: Seq Collection -> Collection -> Bool
noStrictSubset ss s = not (any (isStrictSubset' s) ss)

-- | `isStrictSubset s s'` returns `True` if `s` is a strict subset of `s'`.
isStrictSubset :: Collection -> Collection -> Bool
isStrictSubset s s' = (all go s) && (totalQuantity s < totalQuantity s')
  where
    go :: StampSet -> Bool
    go x = any og s'
      where
        og :: StampSet -> Bool
        og y = (price x == price y) && (quantity x <= quantity y)

-- | Same as `isStrictSubset`, but with the arguments reversed:
-- `isStrictSubset' s' s` returns `True` if `s` is a strict subset of
-- `s'`.
isStrictSubset' :: Collection -> Collection -> Bool
isStrictSubset' = flip isStrictSubset

-- | Turn a collection into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation. The argument 'dp' is
-- used to render the prices of the stamps as decimal values, and not integral
-- ones.
reprCollection :: Int -> Collection -> String
reprCollection dp xs = fmtAsFloat (totalValue xs) $ " EUR (" ++ stamps ++ ")"
  where
    fmtAsFloat :: Int -> ShowS
    fmtAsFloat a = showFFloat (Just 2) a'
      where
        a' :: Double
        a' = ((fromIntegral a) / (10**(fromIntegral dp)))

    stamps = intercalate ", " (toList (fmap go xs))

    go :: StampSet -> String
    go s = (show (quantity s)) ++ "x at " ++ (fmtAsFloat (price s) $ " EUR")

-- | Read a sequence of stamp sets from a CSV-like bytestring.
fromByteString :: Bool -> Int -> BL.ByteString -> Either String Collection
fromByteString comma dp bs = case Csv.decodeByNameWith myOptions bs of
  Left msg -> Left msg
  Right (_, x) -> Right (fmap go ((fromList . V.toList) x))
  where
    myOptions = if comma
      then Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
      else Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }

    go :: CsvStampSet -> StampSet
    go (CsvStampSet p q) = StampSet (round (p * 10**(fromIntegral dp))) q

-- | Read a sequence of stamp sets from a CSV file.
readInventoryFile :: Bool -> Int -> String -> IO (Either String Collection)
readInventoryFile comma dp fname = do
  csvData <- BL.readFile fname
  return (fromByteString comma dp csvData)

-- | Read a sequence of stamp sets from a CSV-like string.
readInventoryString :: Bool -> Int -> String -> Either String (Seq StampSet)
readInventoryString comma dp csvData = fromByteString comma dp (encodeUtf8 . T.pack $ csvData)
