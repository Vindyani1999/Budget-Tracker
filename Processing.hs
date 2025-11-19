-- File: Processing.hs
module Processing
  ( parseTransaction
  , balance
  , monthlySummary
  , categorySummary
  , rollingBalance
  , projectedBalance
  ) where

import DataTypes
import Data.Time (Day, toGregorian)
import Data.List (foldl', groupBy, sortOn)
import Data.Function (on)


-- | Parse CSV fields into Transaction. Expect fields: date,amount,category,note
-- date in YYYY-MM-DD
parseTransaction :: (Day, String, String, String) -> Either String Transaction
parseTransaction (d, amtS, cat, note) =
  case reads amtS :: [(Double, String)] of
    [(a, "")] -> Right $ Transaction d a cat note
    _ -> Left $ "Invalid amount: " ++ amtS


-- | Calculate total balance from transactions
balance :: [Transaction] -> Double
balance = foldl' (\acc t -> acc + tAmount t) 0.0


-- | Produce a monthly summary for a list of transactions
monthlySummary :: [Transaction] -> [Summary]
monthlySummary ts =
  let keyed = groupBy ((==) `on` monthKey) $ sortOn monthKey ts
  in map summarize keyed
  where
    monthKey t = let (y, m, _) = toGregorian (tDate t) in (y, m)
    summarize grp =
      case grp of
        (first:_) ->
          let (y, m, _) = toGregorian (tDate first)
              period = show y ++ "-" ++ (if m < 10 then '0' : show m else show m)
              income = sum [a | Transaction {tAmount = a} <- grp, a > 0]
              expense = negate $ sum [a | Transaction {tAmount = a} <- grp, a < 0]
              net = income - expense
          in Summary period income expense net
        [] -> Summary "" 0 0 0


-- | Sum by category
categorySummary :: [Transaction] -> [(String, Double)]
categorySummary ts =
  let grouped = groupBy ((==) `on` tCategory) $ sortOn tCategory ts
  in map (\g -> case g of
                   (x:_) -> (tCategory x, sum (map tAmount g))
                   [] -> ("", 0)) grouped


-- | Rolling balance over time (sorted by date)
rollingBalance :: [Transaction] -> [(Day, Double)]
rollingBalance ts =
  let sorted = sortOn tDate ts
  in snd $ foldl' go (0.0, []) sorted
  where
    go (acc, xs) t = let acc' = acc + tAmount t in (acc', xs ++ [(tDate t, acc')])


-- | Simple projection: average monthly net * n months + current balance
projectedBalance :: Double -> [Transaction] -> Int -> Double
projectedBalance current ts months =
  let monthly = monthlySummary ts
      avgNet = if null monthly then 0 else (sum (map sNet monthly)) / fromIntegral (length monthly)
  in current + avgNet * fromIntegral months