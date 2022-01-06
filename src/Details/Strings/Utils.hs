module Details.Strings.Utils(
    offsetPastEndl,
    offsetPastTabsSpaces,
    pastEndl,
    pastTabsSpaces,
    startsWithEndl,
    offsetPastChar,
    offsetPastStr,
    offsetAtStr,
    pastChar,
    beforeChar,
    startsWith,
    pastOffset,
    beforeOffset) where

import Data.List (isPrefixOf)

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

startsWithEndl :: String -> Bool
startsWithEndl ('\r':'\n':_) = True
startsWithEndl ('\n':_) = True
startsWithEndl _ = False

startsWith :: String -> Char -> Bool
startsWith [] _ = False
startsWith xs c = head xs == c

offsetPastChar :: Char -> String -> Int
offsetPastChar _ [] = 0
offsetPastChar c (x:xs)
  | x == c = 1
  | otherwise = 1 + offsetPastChar c xs

offsetPastStr :: String -> String -> Int
offsetPastStr xs ys
    | offsetBefore == 0 = 0
    | otherwise = length xs + offsetBefore
    where
        offsetBefore = offsetAtStr xs ys

offsetAtStr :: String -> String -> Int
offsetAtStr _ [] = 0
offsetAtStr xs ys
    | xs `isPrefixOf` ys = 0
    | otherwise = 1 + offsetAtStr xs (tail ys)

pastTabsSpaces :: String -> (Int, String)
pastTabsSpaces = pastOffset offsetPastTabsSpaces

pastEndl :: String -> (Int, String)
pastEndl = pastOffset offsetPastEndl

pastChar :: Char -> String -> (Int, String)
pastChar c = pastOffset (offsetPastChar c)

beforeChar :: Char -> String -> (Int, String)
beforeChar c = beforeOffset (offsetPastChar c)

pastOffset = tupWithOffsetFunc drop
beforeOffset = tupWithOffsetFunc take

tupWithOffsetFunc :: (Int -> [a] -> [a]) -> ([a] -> Int) -> [a] -> (Int, [a]) 
tupWithOffsetFunc f g xs = (i, f i xs) 
    where i = g xs