module StrUtils(skipTabsSpaces, skipToEndl) where

skipTabsSpaces :: String -> Int
skipTabsSpaces [] = 0
skipTabsSpaces (x:xs) | x == '\t' || x == ' ' = 1 + skipTabsSpaces xs
                      | otherwise = 0

skipToEndl :: String -> Int
skipToEndl [] = 0
skipToEndl (x : y : xs)
  | x == '\r' && y == '\n' = 2
  | x == '\n' = 1
  | otherwise = 1 + skipToEndl (y:xs)
skipToEndl [_] = 1