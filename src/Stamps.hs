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
  changeQuantity,
  mkRange,
  empty,
  emptyLike,
  add,
  fromList,
  collectionToList,
  crease,
  noStrictSubset,
  reprCollection,
  fromByteString,
  readInventoryFile,
  readInventoryString,
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Char ( ord )
import qualified Data.Csv as Csv
import Data.List ( intercalate )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as M
import Data.Sequence ( Seq(..) )
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

-- | A collection of stamps.
data Collection = Col { content :: IntMap Int
                      , decimalPlaces :: Int
                      }
  deriving (Eq, Ord, Show)

-- | Create a set of stamp.
mkStampSet :: Int -> Double -> Int -> Maybe StampSet
mkStampSet dp p q = if (dp >= 0) && (p > 0) && (q >= 0)
                    then Just (StampSet { price_ = fromDouble dp p
                                        , quantity_ = q
                                        , decimalPlaces_ = dp
                                        })
                    else Nothing

-- | An empty collection of stamps.
empty :: Int -> Collection
empty dp = Col { content = M.empty
               , decimalPlaces = dp
               }

-- | An empty collection of stamps, which has the same precision for
-- stamp prices has the given collection.
emptyLike :: Collection -> Collection
emptyLike xs = empty (decimalPlaces xs)

-- | Add a set to a collection.
add :: StampSet -> Collection -> Collection
add s xs = if (decimalPlaces_ s) /= (decimalPlaces xs)
           then error "Malformed data!" -- FIXME: improve the error message
           else if (p > 0) && (q > 0)
                then xs { content = M.insertWith (+) p q (content xs) }
                else xs
  where
    p = price_ s
    q = quantity s

-- | Turn a list of stamp sets into a collection.
fromList :: Int -> [StampSet] -> Collection
fromList dp xs = foldr add (empty dp) xs

-- | Turn a collection into a list of stamp sets.
collectionToList :: Collection -> [StampSet]
collectionToList xs = fmap ( \ (p, i) -> StampSet { price_ = p
                                                  , quantity_ = i
                                                  , decimalPlaces_ = decimalPlaces xs } ) (M.toList $ content xs)

-- | A `fold` for collections.
crease :: (StampSet -> b -> b) -> b -> Collection -> b
crease f y0 xs = M.foldrWithKey go y0 (content xs)
  where
    go p q y = f s y
      where
        s = StampSet { price_ = p
                     , quantity_ = q
                     , decimalPlaces_ = decimalPlaces xs
                     }

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

-- | Get the total value of a set of stamps.
setValue :: StampSet -> Double
setValue s = toDouble (decimalPlaces_ s) ((price_ s) * (quantity_ s))

-- | Get the total value of a collection of stamps.
totalValue :: Collection -> Double
totalValue xs = toDouble (decimalPlaces xs) total
  where
    total = (sum . (fmap (\ (p, q) -> p * q)) . M.toList . content) xs

-- | Get the total number of stamps in a collection of stamps.
totalQuantity :: Collection -> Int
totalQuantity xs = (sum . M.elems . content) xs

-- | Change the quantity in a set.
changeQuantity :: StampSet -> Int -> StampSet
changeQuantity x i = x { quantity_ = i }

-- | Create a list of sets of length `1 + quantity s`. The first set
-- contains zero stamp, the second one, etc, and the last one `quantity s`.
mkRange :: StampSet -> [StampSet]
mkRange s = [ StampSet { price_ = price_ s
                       , quantity_ = i
                       , decimalPlaces_ = decimalPlaces_ s
                       }
            | i <- [0..(quantity s)]
            ]

-- | `noSubset ss s` returns `True` if `s` has no strict subset in `ss`.
noStrictSubset :: Seq Collection -> Collection -> Bool
noStrictSubset ss s = not (any (isStrictSubset' s) ss)

-- | `isStrictSubset s s'` returns `True` if `s` is a strict subset of `s'`.
isStrictSubset :: Collection -> Collection -> Bool
isStrictSubset s s' =
  (all go (M.toList . content $ s)) && (totalQuantity s < totalQuantity s')
  where
    go :: (Int, Int) -> Bool
    go (p, q) = any og (M.toList . content $ s')
      where
        og :: (Int, Int) -> Bool
        og (p', q') = (p == p') && (q <= q')

-- | Same as `isStrictSubset`, but with the arguments reversed:
-- `isStrictSubset' s' s` returns `True` if `s` is a strict subset of
-- `s'`.
isStrictSubset' :: Collection -> Collection -> Bool
isStrictSubset' = flip isStrictSubset

-- | Turn a collection into a human readable string. The resulting string is not
-- exhaustive and should not be used for serialisation. The argument 'dp' is
-- used to render the prices of the stamps as decimal values, and not integral
-- ones.
reprCollection :: Collection -> String
reprCollection xs =
  showFFloat (Just dp) (totalValue xs) $ " EUR (" ++ stamps ++ ")"
  where
    dp = decimalPlaces xs

    fmtAsFloat :: Int -> ShowS
    fmtAsFloat a = showFFloat (Just dp) (toDouble dp a)

    stamps = intercalate ", " (fmap go (M.toDescList . content $ xs))

    go :: (Int, Int) -> String
    go (p, q) = (show q) ++ "x at " ++ (fmtAsFloat p $ " EUR")

-- | Read a collection of stamps from a CSV-like bytestring.
fromByteString :: Bool -> Int -> BL.ByteString -> Either String Collection
fromByteString comma dp bs = case Csv.decodeByNameWith myOptions bs of
  Left msg -> Left msg
  Right (_, x) -> let pairs = fmap (toPair dp) (V.toList x)
                  in Right (Col { content = foldr go M.empty pairs
                                , decimalPlaces = dp
                                })
  where
    myOptions = if comma
      then Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ',') }
      else Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }

    go :: (Int, Int) -> IntMap Int -> IntMap Int
    go (p, q) dict = case M.lookup p dict of
      Nothing -> M.insert p q dict
      Just q0 -> M.insert p (q0 + q) dict

-- | Convertit un CsvStampSet en une paire d'entiers
toPair :: Int -> CsvStampSet -> (Int, Int)
toPair dp (CsvStampSet p q) = (fromDouble dp p, q)

-- | Read a collection of stamps from a CSV file.
readInventoryFile :: Bool -> Int -> String -> IO (Either String Collection)
readInventoryFile comma dp fname = do
  csvData <- BL.readFile fname
  return (fromByteString comma dp csvData)

-- | Read a collection of stamps from a CSV-like string.
readInventoryString :: Bool -> Int -> String -> Either String Collection
readInventoryString comma dp csvData = fromByteString comma dp (encodeUtf8 . T.pack $ csvData)
