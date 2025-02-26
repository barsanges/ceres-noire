{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Stamps
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Stamps and collections of stamps.
-}

module Stamps (
  precision,
  StampSet(..),
  totalValue,
  totalQuantity,
  sortCollections,
  noStrictSubset,
  reprCollection,
  fromByteString,
  readInventoryFile,
  readInventoryString,
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char ( ord )
import qualified Data.Csv as Csv
import Data.List ( groupBy, intercalate, sortBy )
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding ( encodeUtf8 )
import qualified Data.Vector as V
import Numeric ( showFFloat )

-- | The precision used for numerical comparison.
precision :: Double
precision = 1e-9

-- | A set of similar stamps is defined by the unitary price of the
-- stamp, and the quantity of stamps in the set. Both these quantities
-- should be positive.
data StampSet = St { price :: Double
                   , quantity :: Int
                   }
  deriving Show

instance Csv.FromNamedRecord StampSet where
  parseNamedRecord r = do
    p <- Csv.lookup r "price"
    q <- Csv.lookup r "quantity"
    if (p > 0) && (q >= 0)
      then return St { price = p, quantity = q }
      else fail "Invalid data!" -- FIXME: improve the error message.

-- | Get the total value of a collection of stamps.
totalValue :: [StampSet] -> Double
totalValue xs = sum (fmap (\ x -> price x * (fromIntegral $ quantity x)) xs)

-- | Get the total number of stamps in a collection of stamps.
totalQuantity :: [StampSet] -> Int
totalQuantity xs = sum (fmap quantity xs)

-- | Compare two sets.
compareSets :: StampSet -> StampSet -> Ordering
compareSets x y = if abs (price x - price y) < precision
                  then compare (quantity y) (quantity x)
                  else compare (price y) (price x)

-- | Compare two collections: the "smallest" is the one with first the
-- lowest total value, then the lowest number of stamps, and finally
-- the most expensive ones.
compareCollection :: [StampSet] -> [StampSet] -> Ordering
compareCollection xs ys
  | abs (yval - xval) > precision = compare yval xval
  | totalQuantity ys < totalQuantity xs = GT
  | totalQuantity ys == totalQuantity xs = recursiveCompare xs ys
  | otherwise = LT
  where
    xval = totalValue xs
    yval = totalValue ys

    recursiveCompare :: [StampSet] -> [StampSet] -> Ordering
    recursiveCompare [] [] = EQ
    recursiveCompare [] _ = LT
    recursiveCompare _ [] = GT
    recursiveCompare (u:us) (v:vs) = case compareSets u v of
                                       GT -> GT
                                       LT -> LT
                                       EQ -> recursiveCompare us vs

-- | Sort each collection in a list of collections, and sort the
-- resulting list.
sortCollections :: [[StampSet]] -> [[StampSet]]
sortCollections = (sortBy compareCollection) . (fmap (sortBy compareSets))

-- | `noSubset ss s` returns `True` if `s` has no strict subset in `ss`.
noStrictSubset :: [[StampSet]] -> [StampSet] -> Bool
noStrictSubset ss s = not (any ((flip isStrictSubset) s) ss)

-- | `isStrictSubset s s'` returns `True` if `s` is a strict subset of `s'`.
isStrictSubset :: [StampSet] -> [StampSet] -> Bool
isStrictSubset s s' =
  (all go s) && (totalQuantity s < totalQuantity s')
  where
    go :: StampSet -> Bool
    go u = any og s'
      where
        og :: StampSet -> Bool
        og v = ((abs (price u - price v) < precision)) && (quantity u <= quantity v)

-- | Turn a collection into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation. The argument 'dp' is
-- used to render the prices of the stamps as decimal values, and not integral
-- ones.
reprCollection :: Int -> [StampSet] -> String
reprCollection decimals xs =
  showFFloat (Just decimals) (totalValue xs) $ " EUR (" ++ stamps ++ ")"
  where
    stamps = intercalate ", " (fmap go xs)
    go :: StampSet -> String
    go u = (show (quantity u)) ++ "x at " ++ (showFFloat (Just decimals) (price u) $ " EUR")

-- | Read a collection of stamps from a CSV-like bytestring.
fromByteString :: Bool -> BL.ByteString -> Either String [StampSet]
fromByteString comma bs = case Csv.decodeByNameWith myOptions bs of
  Left msg -> Left msg
  Right (_, x) -> let stamps = fmap go2 $ groupBy go1 $ sortBy compareSets $ V.toList x
                  in Right stamps
  where
    go2 :: [StampSet] -> StampSet
    go2 [] = St { price = 0, quantity = 0 }
    go2 (x:xs) = x { quantity = quantity x + (foldr (\ y t -> quantity y + t) 0 xs) }

    go1 :: StampSet -> StampSet -> Bool
    go1 x y = abs ((price x) - (price y)) < precision

    myOptions = if comma
      then Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
      else Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }

-- | Read a collection of stamps from a CSV file.
readInventoryFile :: Bool -> String -> IO (Either String [StampSet])
readInventoryFile comma fname = do
  csvData <- BL.readFile fname
  return (fromByteString comma csvData)

-- | Read a collection of stamps from a CSV-like string.
readInventoryString :: Bool -> String -> Either String [StampSet]
readInventoryString comma csvData = fromByteString comma (encodeUtf8 . T.pack $ csvData)
