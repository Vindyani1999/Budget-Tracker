-- File: DataTypes.hs
module DataTypes where

import Data.Time (Day)


-- | A transaction in the budget tracker.
-- date: YYYY-MM-DD
data Transaction = Transaction
  { tDate :: Day
  , tAmount :: Double 
  , tCategory :: String
  , tNote :: String
  } deriving (Eq, Show)


-- | A simple report summary
data Summary = Summary
  { sPeriod :: String
  , sTotalIncome :: Double
  , sTotalExpense :: Double
  , sNet :: Double
  } deriving (Eq, Show)