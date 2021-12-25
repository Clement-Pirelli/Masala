module Tokeniser(TokenPosition, LinePosition, PPLiteral, TokenContents, Token, tokenise) where

import Data.List(foldl')
import Data.Char(isSpace)
import Data.Maybe (fromJust, isNothing, isJust)

--todo: take these out of here

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs


indexedBy :: ((Int, a) -> Int) -> [a] -> [(Int, a)]
indexedBy f = foldl' step []
    where step o x | null o = [(0, x)]
                   | otherwise = o ++ [(f (last o), x)]

indexed :: [a] -> [(Int, a)]
indexed = indexedBy step
    where step o = 1 + fst o

makeIndexedLines :: String -> [(Int, String)]
makeIndexedLines = indexed . lines
--

data LinePosition = LinePosition {
      lineNumber :: Int
    , lineContents :: String
    } deriving (Show)

data TokenPosition = TokenPosition {
      linePosition :: LinePosition
    , charNumber :: Int
    , tokContents :: String
    } deriving (Show)

data PPLiteral = PPString String
    | PPFloat Double
    | PPDouble Double
    | PPInt Int
    deriving (Show)

data UnaryOperatorType = Not | HasAttribute | HasCAttribute | HasCPPAttribute | HasBuiltin | HasInclude | Defined deriving (Show, Eq)
data BinaryOperatorType = GreaterEqual | Greater | Equal | And | Or | IsNot | LesserEqual | Lesser deriving (Show, Eq)

data TokenContents = If
    | Elif
    | Ifdef
    | Else
    | Endif
    | Define
    | Pragma
    | Symbol
    | Include
    | UnaryOperator UnaryOperatorType
    | BinaryOperator BinaryOperatorType
    | Literal PPLiteral
    | OpeningParens
    | ClosingParens
    | Comma
    | Unknown
    deriving (Show)

data Token = Token {
      tokenContents :: TokenContents
    , tokenPosition :: TokenPosition
    } deriving (Show)

directiveTokens :: [(String, TokenContents)]
directiveTokens = [
        ("#include", Include),
        ("#define", Define),
        ("#pragma", Pragma),
        ("#ifdef", Ifdef),
        ("#endif", Endif),
        ("#elif", Elif),
        ("#else", Else),
        ("#if", If)
    ]

bodyTokens :: [(String, TokenContents)]
bodyTokens = [
        ("__has_attribute", UnaryOperator HasAttribute),
        ("__has_c_attribute", UnaryOperator HasCAttribute),
        ("__has_cpp_attribute", UnaryOperator HasCPPAttribute),
        ("__has_builtin", UnaryOperator HasBuiltin),
        ("__has_include", UnaryOperator HasInclude),
        ("defined", UnaryOperator Defined),
        (">=", BinaryOperator GreaterEqual),
        ("<=", BinaryOperator LesserEqual),
        ("==", BinaryOperator Equal),
        ("&&", BinaryOperator And),
        ("!=", BinaryOperator IsNot),
        ("||", BinaryOperator Or),
        ("(", OpeningParens),
        (")", ClosingParens),
        (",", Comma),
        ("!", UnaryOperator Not),
        (">", BinaryOperator Greater),
        ("<", BinaryOperator Lesser)
    ]

nextBodyTokenPosition :: TokenPosition -> Maybe TokenPosition
nextBodyTokenPosition (TokenPosition line@(LinePosition lineNum contents) charNum tokStr) = 
    if null (dropWhile isSpace newTokenStart) then Nothing else Just $ TokenPosition line (charNum + tokLength) newTokenStart
    where
        newPosition = TokenPosition line (charNum + tokLength) newTokenStart
        newTokenStart = drop tokLength contents
        tokLength = length tokStr


findBodyTokens :: Maybe TokenPosition -> [Token]
findBodyTokens Nothing = []
findBodyTokens (Just pos) = undefined

findDirectiveToken :: TokenPosition -> Maybe Token
findDirectiveToken (TokenPosition line charNum tokStr) = do
    contents <- lookup tokStr directiveTokens
    let tokLength = length tokStr
    let newTokenStart = drop tokLength $ lineContents line
    let newPosition = TokenPosition line (charNum + tokLength) newTokenStart
    return $ Token contents newPosition

findTokens :: LinePosition -> Maybe [Token]
findTokens linePos@(LinePosition lineNum str) = do
    let whitespaceSpan = span isSpace str
    let tokenStr = snd whitespaceSpan
    let startTokCharCount = length (fst whitespaceSpan)
    tokStart <- (safeHead . words) tokenStr
    let directiveTokenPosition = TokenPosition linePos startTokCharCount tokStart
    directiveToken <- findDirectiveToken directiveTokenPosition
    return $ directiveToken : findBodyTokens (nextBodyTokenPosition directiveTokenPosition)

tokenise :: String -> [Token]
tokenise input = undefined
    where
        inputLines = makeIndexedLines input