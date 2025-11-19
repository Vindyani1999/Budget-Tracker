-- File: Main.hs
module Main where

import IOHandler (runInteractive)


main :: IO ()
main = do

  putStrLn "Budget Tracker (Haskell) - Starting..."
  runInteractive

