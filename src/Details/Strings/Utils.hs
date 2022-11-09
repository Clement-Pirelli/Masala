module Details.Strings.Utils(
    offsetPastEndl,
    offsetPastTabsSpaces,
    pastEndl,
    pastTabsSpaces,
    startsWithEndl,
    offsetPastChar,
    offsetAtStr,
    offsetPastString,
    pastString,
    pastChar,
    beforeChar,
    startsWith,) where

import Details.ListUtils ( pastOffset, beforeOffset )
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

offsetPastString :: String -> String -> Int
offsetPastString _ [] = 0
offsetPastString xs ys = length xs + offsetAtStr xs ys

offsetAtStr :: String -> String -> Int
offsetAtStr _ [] = 0
offsetAtStr xs ys
    | xs `isPrefixOf` ys = 0
    | otherwise = 1 + offsetAtStr xs (tail ys)

pastTabsSpaces :: String -> (Int, String)
pastTabsSpaces = pastOffset offsetPastTabsSpaces

pastEndl :: String -> (Int, String)
pastEndl = pastOffset offsetPastEndl

pastString :: String -> String -> (Int, String)
pastString s = pastOffset (offsetPastString s)

pastChar :: Char -> String -> (Int, String)
pastChar c = pastOffset (offsetPastChar c)

beforeChar :: Char -> String -> (Int, String)
beforeChar c = beforeOffset (offsetPastChar c)
