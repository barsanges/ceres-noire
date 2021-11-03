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

-- | How the results of the algorithm should be used?
data Usage = Ask | DryRun | Yes | Variants
  deriving Eq

-- | Format of the inventory: CSV file or CSV-like string.
data Input = Fin String | Str String

-- | Command line arguments.
data Args = Args { totalCost :: Double
                 , input :: Input
                 , usage :: Usage
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
  <*> ( ( flag Ask DryRun
          ( long "dry-run"
          <> help "If this option is used, the inventory file is not\
                  \ updated (useless if the option '--string' is used)" ))
      <|> ( flag' Yes
            ( long "yes"
            <> short 'y'
            <> help "If this option is used, the program does not\
                    \ ask for confirmation before updating the\
                    \ inventory file  (useless if the option\
                    \ '--string' is used)" ))
      <|> ( flag' Variants
            ( long "variants"
            <> help "If this option is used, the program returns\
                    \ both the optimal solutions and several\
                    \ (if possible) suboptimal ones" )))
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
      Right opt -> do
        (putStrLn . reprSolution) opt
        case (usage cli, input cli) of
          (Ask, Fin f) -> confirm "Do you want to update the inventory (Y/n)?" (update f opt)
          (Yes, Fin f) -> (update f opt)
          (Variants, _) -> do
            putStrLn "Variants:"
            (putStrLn . reprVariants) (variants opt)
          _ -> return ()
      where
        update f x = writeInventoryFile (comma cli) f (resultingInventory x)

-- | Ask for confirmation before performing an action.
confirm :: String -> IO () -> IO ()
confirm msg a = do
  putStrLn msg
  x <- getLine
  if (x == "Y") || (x == "y")
  then a
  else return ()
