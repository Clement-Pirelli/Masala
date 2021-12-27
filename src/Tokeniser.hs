module Tokeniser(scanTokens) where

import Data.Maybe(fromJust, isNothing, isJust)

import Token

import StrUtils(skipToEndl, skipTabsSpaces)

scanDirectiveBody :: String -> (Int, [Token])
scanDirectiveBody xs = (skipToEndl xs, [])

scanDirective :: Int -> Int -> String -> Bool -> Maybe (Int, Token)
scanDirective l c xs spaceBefore = do
    tok <- xs `atStartOf` directiveTokens
    let lexm = fst tok
    let len = length lexm 
    return (len, Token { tokenType = snd tok, lexeme = lexm, literal = Nothing, line = l, character = c, preceededBySpace = spaceBefore })

onNoDirective :: Int -> Int -> [Char] -> [Token]
onNoDirective l c afterWhiteSpace
    | head afterWhiteSpace == '#' = error $ "Unrecognized preprocessor directive at line " ++ show l
    | otherwise = scanTokens' True (l+1) (c+lineEnd) afterLineEnd
    where
        afterLineEnd = drop lineEnd afterWhiteSpace
        lineEnd = skipToEndl afterWhiteSpace

onDirectiveToken :: Int -> Int -> (Int, Token) -> String -> [Token]
onDirectiveToken l c directiveTok afterWhiteSpace = snd directiveTok : snd bodyToks ++ rest
    where
        rest = scanTokens' False (l+1) (c+bodyLen) (drop bodyLen bodyStr)
        bodyLen = fst bodyToks
        bodyToks = scanDirectiveBody bodyStr
        bodyStr = drop directiveEnd afterWhiteSpace
        directiveEnd = fst directiveTok


scanTokens' :: Bool -> Int -> Int -> String -> [Token]

scanTokens' spaceBefore l c [] = [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, line = l, character = c, preceededBySpace = spaceBefore }]
--todo: make each branch its own function tbqh
scanTokens' spaceBefore l c xs
    | noDirective = onNoDirective l (c+startWhiteSpace) afterWhiteSpace
    | otherwise = onDirectiveToken l (c + startWhiteSpace + fst directiveTok') directiveTok' afterWhiteSpace
    where
        directiveTok' = fromJust directiveTok
        noDirective = isNothing directiveTok
        directiveTok = scanDirective l (c+startWhiteSpace) afterWhiteSpace beforeDirectiveIsWhiteSpace
        beforeDirectiveIsWhiteSpace = startWhiteSpace > 0
        afterWhiteSpace = drop startWhiteSpace xs
        startWhiteSpace = skipTabsSpaces xs

scanTokens :: String -> [Token]
scanTokens = scanTokens' False 0 0