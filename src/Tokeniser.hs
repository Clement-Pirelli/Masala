module Tokeniser(scanTokens) where

import Data.Maybe(fromJust, isNothing, isJust)

import Token

import StrUtils(skipToEndl, skipTabsSpaces)

scanDirectiveBody :: String -> (Int, [Token])
scanDirectiveBody xs = (skipToEndl xs, [])

scanDirective :: Int -> String -> Maybe (Int, Token)
scanDirective l xs = do
    tok <- xs `atStartOf` directiveTokens
    return (length $ fst tok, Token { tokenType = snd tok, lexeme = fst tok, literal = Nothing, line = l })

onNoDirective :: Int -> [Char] -> [Token]
onNoDirective l afterWhiteSpace = scanTokens' (l+1) afterLineEnd
    where
        afterLineEnd = drop lineEnd afterWhiteSpace
        lineEnd = skipToEndl afterWhiteSpace

onDirectiveToken l directiveTok afterWhiteSpace = snd directiveTok : snd bodyToks ++ scanTokens' (l+1) (drop (fst bodyToks) bodyStr)
    where
        bodyToks = scanDirectiveBody bodyStr
        bodyStr = drop directiveEnd afterWhiteSpace
        directiveEnd = fst directiveTok


scanTokens' :: Int -> String -> [Token]
scanTokens' l [] = [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, line = l }]
--todo: make each branch its own function tbqh
scanTokens' l xs | noDirective = onNoDirective l afterWhiteSpace
                 | otherwise = onDirectiveToken l (fromJust directiveTok) afterWhiteSpace
    where
        noDirective = isNothing directiveTok
        directiveTok = scanDirective l afterWhiteSpace
        afterWhiteSpace = drop startWhiteSpace xs
        startWhiteSpace = skipTabsSpaces xs

scanTokens :: String -> [Token]
scanTokens = scanTokens' 0