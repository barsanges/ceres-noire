{- |
   Module      : Main
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Parses command-line options and solves the postage cost problem.
-}

module Main where

import Options.Applicative
import Stamps
import Algorithm

-- | Format of the inventory: CSV file or CSV-like string.
data Input = Fin String | Str String

-- | Command line arguments.
data Args = Args { low :: Double
                 , up :: Double
                 , nstamps :: Int
                 , input :: Input
                 , comma :: Bool
                 , decimalPlaces :: Int
                 }

argsParser :: Parser Args
argsParser = Args
  <$> ( argument auto
        ( metavar "MIN"
        <> help "Minimal cost of the letter in euros" ))
  <*> ( argument auto
        ( metavar "MAX"
        <> help "Maximal cost of the letter in euros" ))
  <*> ( argument auto
        ( metavar "NSTAMPS"
        <> help "Maximal number of stamps that can be used in the solution" ))
  <*> ( ( Fin <$> strOption
          ( long "file"
          <> short 'f'
          <> metavar "FNAME"
          <> help "CSV file containing a stamps inventory (unitary\
                  \ prices and available quantities)" ))
      <|> ( Str <$> strOption
            ( long "string"
            <> short 's'
            <> metavar "STRING"
            <> help "CSV-like string containing a stamps inventory (unitary\
                    \ prices and available quantities)" )))
  <*> ( switch
        ( long "comma"
        <> short 'c'
        <> help "use commas instead of semi-colons as CSV separator" ))
  <*> (option auto
       ( long "decimal-places"
         <> short 'd'
         <> metavar "DECIMAL_PLACES"
         <> value 2
         <> showDefault
         <> help "number of decimal places to use"
       ))

-- | Command line parser for 'ceres-noire'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "cn -- cérès noire"
  <> progDesc "Choose the right stamps combination for your letter based on a\
              \ given stamps inventory"
  <> footer "This program is licensed under the GNU GPL 3 (see\
            \ www.gnu.org/licenses/). It comes with absolutely no\
            \ warranty. This is free software, and you are welcome to\
            \ redistribute it under certain conditions.")

-- | Application entry point.
main :: IO ()
main = do
  cli <- execParser args
  let dp = decimalPlaces cli
  maybeInventory <- case input cli of
      Fin fin -> readInventoryFile (comma cli) fin
      Str s -> pure (readInventoryString (comma cli) s)
  case maybeInventory of
    Left err -> putStrLn err
    Right inventory -> case withinRange (Just (nstamps cli)) (low cli) (up cli) inventory of
      Left msg -> putStrLn msg
      Right res -> putStrLn (reprCollections dp (dropSupersets res))
