-- File: Utils.hs
module Utils where

import Data.Time (Day)


-- Utilities kept intentionally tiny for testability


-- | Safe head
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x


-- | Chunk a list by size (pure)
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)


-- | Convert boolean to 1/0
boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0