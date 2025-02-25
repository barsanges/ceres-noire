{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Stamps
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Stamps and collections of stamps.
-}

module Stamps (
  StampSet,
  mkStampSet,
  price,
  quantity,
  totalValue,
  totalQuantity,
  changeQuantity,
  noStrictSubset,
  reprCollection,
  fromByteString,
  readInventoryFile,
  readInventoryString,
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char ( ord )
import qualified Data.Csv as Csv
import Data.List ( groupBy, intercalate )
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Vector as V
import Numeric ( showFFloat )

-- | A set of similar stamps is defined by the unitary price of the stamp, and
-- the quantity of stamps in the set.
data StampSet = StampSet { price_ :: Int
                         , quantity_ :: Int
                         , decimalPlaces_ :: Int
                         }
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

-- | Create a set of stamp.
mkStampSet :: Int -> Double -> Int -> Maybe StampSet
mkStampSet dp p q = if (dp >= 0) && (p > 0) && (q >= 0)
                    then Just (StampSet { price_ = fromDouble dp p
                                        , quantity_ = q
                                        , decimalPlaces_ = dp
                                        })
                    else Nothing

-- | Convert a double to an int, preserving a given decimal precision.
fromDouble :: Int -> Double -> Int
fromDouble dp x = round (x * 10**(fromIntegral dp))

-- | Convert an int to a double with a given decimal precision.
toDouble :: Int -> Int -> Double
toDouble dp n = (fromIntegral n) / (10**(fromIntegral dp))

-- | Get the unitary price of a stamp.
price :: StampSet -> Double
price s = toDouble (decimalPlaces_ s) (price_ s)

-- | Get the available number of stamps in a set of stamps.
quantity :: StampSet -> Int
quantity s = quantity_ s

-- | Get the total value of a collection of stamps.
totalValue :: [StampSet] -> Double
totalValue xs = sum (fmap (\ x -> price x * (fromIntegral $ quantity x)) xs)

-- | Get the total number of stamps in a collection of stamps.
totalQuantity :: [StampSet] -> Int
totalQuantity xs = sum (fmap quantity xs)

-- | Change the quantity in a set.
changeQuantity :: StampSet -> Int -> StampSet
changeQuantity x i = x { quantity_ = i }

-- | `noSubset ss s` returns `True` if `s` has no strict subset in `ss`.
noStrictSubset :: [[StampSet]] -> [StampSet] -> Bool
noStrictSubset ss s = not (any (isStrictSubset' s) ss)

-- | `isStrictSubset s s'` returns `True` if `s` is a strict subset of `s'`.
isStrictSubset :: [StampSet] -> [StampSet] -> Bool
isStrictSubset s s' =
  (all go s) && (totalQuantity s < totalQuantity s')
  where
    go :: StampSet -> Bool
    go u = any og s'
      where
        og :: StampSet -> Bool
        og v = ((abs (price u - price v) < 1e-9)) && (quantity u <= quantity v)

-- | Same as `isStrictSubset`, but with the arguments reversed:
-- `isStrictSubset' s' s` returns `True` if `s` is a strict subset of
-- `s'`.
isStrictSubset' :: [StampSet] -> [StampSet] -> Bool
isStrictSubset' = flip isStrictSubset

-- | Turn a collection into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation. The argument 'dp' is
-- used to render the prices of the stamps as decimal values, and not integral
-- ones.
reprCollection :: Int -> [StampSet] -> String
reprCollection precision xs =
  showFFloat (Just precision) (totalValue xs) $ " EUR (" ++ stamps ++ ")"
  where
    stamps = intercalate ", " (fmap go xs)
    go :: StampSet -> String
    go u = (show (quantity u)) ++ "x at " ++ (showFFloat (Just precision) (price u) $ " EUR")

-- | Read a collection of stamps from a CSV-like bytestring.
fromByteString :: Bool -> Int -> BL.ByteString -> Either String [StampSet]
fromByteString comma dp bs = case Csv.decodeByNameWith myOptions bs of
  Left msg -> Left msg
  Right (_, x) -> let stamps = fmap go1 (fmap go2 $ groupBy go3 $ V.toList x)
                  in Right stamps
  where
    go1 :: CsvStampSet -> StampSet
    go1 (CsvStampSet p q) = StampSet { price_ = fromDouble dp p
                                     , quantity_ = q
                                     , decimalPlaces_ = dp
                                     }

    go2 :: [CsvStampSet] -> CsvStampSet
    go2 [] = CsvStampSet 0 0
    go2 ((CsvStampSet p q):xs) = CsvStampSet p (q + (foldr (\ (CsvStampSet _ q') t -> q' + t) 0 xs))

    go3 :: CsvStampSet -> CsvStampSet -> Bool
    go3 (CsvStampSet p _) (CsvStampSet p' _) = abs (p - p') < 1e-9

    myOptions = if comma
      then Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
      else Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }

-- | Read a collection of stamps from a CSV file.
readInventoryFile :: Bool -> Int -> String -> IO (Either String [StampSet])
readInventoryFile comma dp fname = do
  csvData <- BL.readFile fname
  return (fromByteString comma dp csvData)

-- | Read a collection of stamps from a CSV-like string.
readInventoryString :: Bool -> Int -> String -> Either String [StampSet]
readInventoryString comma dp csvData = fromByteString comma dp (encodeUtf8 . T.pack $ csvData)
