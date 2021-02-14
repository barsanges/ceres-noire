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
data Usage = Ask | DryRun | Yes | Boundary
  deriving Eq

-- | Command line arguments.
data Args = Args { totalCost :: Float, fin :: String, usage :: Usage }

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
      <|> ( flag' Boundary
            ( long "boundary"
            <> short 'b'
            <> help "If this option is used, the program returns\
                    \ the boundary of the set of admissible\
                    \ solutions rather than an optimal solution" )))

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
  maybeInventory <- readInventory (fin cli)
  case maybeInventory of
    Left err -> putStrLn err
    Right inventory -> if usage cli == Boundary
      then case (boundary (totalCost cli) inventory) of
        Left msg -> putStrLn msg
        Right res -> (putStrLn . reprBoundary) res
      else case optimum (totalCost cli) inventory of
        Left msg -> putStrLn msg
        Right opt -> do
          (putStrLn . reprSolution) opt
          case (usage cli) of
            Ask -> confirm "Do you want to update the inventory (Y/n)?" (update opt)
            Yes -> (update opt)
            _ -> return ()
      where
        update x = writeInventory (fin cli) (resultingInventory x)

-- | Ask for confirmation before performing an action.
confirm :: String -> IO () -> IO ()
confirm msg a = do
  putStrLn msg
  x <- getLine
  if (x == "Y") || (x == "y")
  then a
  else return ()