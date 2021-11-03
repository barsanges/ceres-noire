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

-- | Command line arguments.
data Args = Args { totalCost :: Double, fin :: String, usage :: Usage, comma :: Bool }

argsParser :: Parser Args
argsParser = Args
  <$> ( argument auto
        ( metavar "COST"
        <> help "Cost of the letter in euros" ))
  <*> ( strOption
        ( long "inventory"
        <> short 'i'
        <> metavar "FNAME"
        -- The default is set manually in the help text because
        -- 'showDefault' gives a weird spacing:
        <> help "CSV file containing a stamps inventory (unitary\
                \ prices and available quantities); default: \
                \'./inventory.csv'"
        <> value "./inventory.csv" ))
  <*> ( ( flag Ask DryRun
          ( long "dry-run"
          <> help "If this option is used, the inventory file is not\
                  \ updated" ))
      <|> ( flag' Yes
            ( long "yes"
            <> short 'y'
            <> help "If this option is used, the program does not\
                    \ ask for confirmation before updating the\
                    \ inventory file" ))
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
  maybeInventory <- readInventoryFile (comma cli) (fin cli)
  case maybeInventory of
    Left err -> putStrLn err
    Right inventory -> case optimum (totalCost cli) inventory of
      Left msg -> putStrLn msg
      Right opt -> do
        (putStrLn . reprSolution) opt
        case (usage cli) of
          Ask -> confirm "Do you want to update the inventory (Y/n)?" (update opt)
          Yes -> (update opt)
          Variants -> do
            putStrLn "Variants:"
            (putStrLn . reprVariants) (variants opt)
          DryRun -> return ()
      where
        update x = writeInventoryFile (comma cli) (fin cli) (resultingInventory x)

-- | Ask for confirmation before performing an action.
confirm :: String -> IO () -> IO ()
confirm msg a = do
  putStrLn msg
  x <- getLine
  if (x == "Y") || (x == "y")
  then a
  else return ()
