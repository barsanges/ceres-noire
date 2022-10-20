{- |
   Module      : Main
   Copyright   : Copyright (C) 2021 barsanges
   License     : GNU GPL, version 3

Parses command-line options and solves the postage cost problem.
-}

module Main where

import Options.Applicative
import StampSet
import Algorithm

-- | Format of the inventory: CSV file or CSV-like string.
data Input = Fin String | Str String

-- | Command line arguments.
data Args = Args { totalCost :: Double
                 , input :: Input
                 , comma :: Bool
                 }

argsParser :: Parser Args
argsParser = Args
  <$> ( argument auto
        ( metavar "COST"
        <> help "Cost of the letter in euros" ))
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

-- | Command line parser for 'ceres-noire'.
args :: ParserInfo Args
args = info (argsParser <**> helper)
  ( fullDesc
  <> header "cn -- cérès noire"
  <> progDesc "Choose the right stamps combination for your letter based on a\
              \ given stamps inventory" )

-- | Application entry point.
main :: IO ()
main = do
  cli <- execParser args
  maybeInventory <- case input cli of
      Fin fin -> readInventoryFile (comma cli) fin
      Str s -> pure (readInventoryString (comma cli) s)
  case maybeInventory of
    Left err -> putStrLn err
    Right inventory -> case optimum (totalCost cli) inventory of
      Left msg -> putStrLn msg
      Right opt -> (putStrLn . reprSolution) opt
