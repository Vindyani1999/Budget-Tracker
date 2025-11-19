-- File: IOHandler.hs
module IOHandler
  ( runInteractive
  , readTransactionsFromCSV
  , writeSummaryToFile
  ) where

import DataTypes
import Processing

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import System.IO
import System.Directory (doesFileExist)
import Control.Exception (catch, IOException)
import Text.Printf (printf)
import Data.Char (toLower)


-- | Read CSV lines and parse to Transaction. CSV: date,amount,category,note
readTransactionsFromCSV :: FilePath -> IO [Transaction]
readTransactionsFromCSV fp = do

  exists <- doesFileExist fp
  if not exists
    then do
      putStrLn $ "File not found: " ++ fp
      return []
    else do
      contents <- readFile fp
      let lsRaw = filter (not . null) $ map strip $ lines contents
          ls = dropHeaderIfPresent lsRaw
          parsed = mapM parseLine ls
      case parsed of
        Left err -> do
          putStrLn $ "Parse error: " ++ err
          return []
        Right txs -> return txs
  where
    strip = unwords . words
    dropHeaderIfPresent xs =
      case xs of
        (h:rest) -> let firstField = case splitComma h of
                                        (f:_) -> map toLower f
                                        [] -> ""
                    in if firstField == "date" then rest else xs
        [] -> []
    parseLine line =
      case splitComma line of
        [dS, aS, c, n] ->
          case parseDate dS of
            Nothing -> Left $ "Invalid date: " ++ dS
            Just d -> case parseTransaction (d, aS, c, n) of
              Left e -> Left e
              Right t -> Right t
        _ -> Left $ "Bad CSV line: " ++ line


-- | Very small CSV splitter (doesn't support quoted commas)
splitComma :: String -> [String]
splitComma s = go s [] where

  go "" acc = reverse acc
  go cs acc =
    let (h, t) = break (== ',') cs
    in case t of
         [] -> reverse (h : acc)
         (_ : rest) -> go rest (h : acc)


parseDate :: String -> Maybe Day
parseDate s = parseTimeM True defaultTimeLocale "%Y-%m-%d" s


-- | Write a human readable summary to a file
writeSummaryToFile :: FilePath -> [Summary] -> IO ()
writeSummaryToFile out s = do

  let txt = unlines $ map fmt s
  writeFile out txt
  where
    fmt (Summary p inc exp net) =
      printf "%s | Income: %.2f | Expense: %.2f | Net: %.2f" p inc exp net


-- | A small interactive flow for demonstration
runInteractive :: IO ()
runInteractive = do

  putStrLn "Enter path to transactions CSV (date,amount,category,note). Press Enter to use sample.csv:"
  fp <- getLine
  let path = if null fp then "sample.csv" else fp
  txs <- readTransactionsFromCSV path
  putStrLn $ "Loaded " ++ show (length txs) ++ " transactions."
  let bal = balance txs
  putStrLn $ "Current balance: " ++ showDouble bal
  putStrLn "Monthly summaries:"
  mapM_ print (monthlySummary txs)
  putStrLn "Category summary (category -> net):"
  mapM_ print (categorySummary txs)
  putStrLn "Projection: enter number of months to project (e.g., 3):"
  nS <- getLine
  let n = case reads nS of
            [(x, "")] -> x
            _ -> 3
  let proj = projectedBalance bal txs n
  putStrLn $ "Projected balance after " ++ show n ++ " months: " ++ showDouble proj
  writeSummaryToFile "report.txt" (monthlySummary txs)
  putStrLn "Wrote monthly report to report.txt"
  where
    showDouble d = (printf "%.2f" d) :: String