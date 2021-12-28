module StrUtils(offsetPastEndl, offsetPastTabsSpaces, pastEndl, pastTabsSpaces, startsWithEndl) where

tupFromOffsetFunc :: ([a] -> Int) -> [a] -> (Int, [a]) 
tupFromOffsetFunc f xs = (i, drop i xs) 
    where i = f xs

offsetPastTabsSpaces :: String -> Int
offsetPastTabsSpaces [] = 0
offsetPastTabsSpaces (x:xs) | x == '\t' || x == ' ' = 1 + offsetPastTabsSpaces xs
                      | otherwise = 0

offsetPastEndl :: String -> Int
offsetPastEndl [] = 0
offsetPastEndl (x : y : xs)
  | x == '\r' && y == '\n' = 2
  | x == '\n' = 1
  | otherwise = 1 + offsetPastEndl (y:xs)
offsetPastEndl [_] = 1

pastTabsSpaces :: String -> (Int, String)
pastTabsSpaces = tupFromOffsetFunc offsetPastTabsSpaces

pastEndl :: String -> (Int, String)
pastEndl = tupFromOffsetFunc offsetPastEndl

startsWithEndl :: String -> Bool
startsWithEndl ('\r':'\n':_) = True
startsWithEndl ('\n':_) = True
startsWithEndl _ = False