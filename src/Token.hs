module Token(module Token, module TokenType, module PPLiteral) where

import TokenType
import PPLiteral

import Data.List(isPrefixOf, find)

data Token = Token {
      tokenType :: TokenType
    , lexeme :: String
    , literal :: Maybe PPLiteral
    , line :: Int
    } deriving (Show)

directiveTokens :: [(String, TokenType)]
directiveTokens = [
        ("#include", TokInclude),
        ("#define", TokDefine),
        ("#pragma", TokPragma),
        ("#ifdef", TokIfdef),
        ("#endif", TokEndif),
        ("#elif", TokElif),
        ("#else", TokElse),
        ("#if", TokIf)
    ]

bodyTokens :: [(String, TokenType)]
bodyTokens = [
        ("true", TokTrue),
        ("false", TokFalse),
        (">=", TokGreaterEqual),
        ("<=", TokLesserEqual),
        ("==", TokEqual),
        ("&&", TokAnd),
        ("!=", TokNotEqual),
        ("||", TokOr),
        ("(", TokOpeningParens),
        (")", TokClosingParens),
        (",", TokComma),
        ("!", TokNot),
        (">", TokClosingChevron),
        ("<", TokOpeningChevron),
        ("&", TokBitAnd),
        ("|", TokBitOr),
        ("+", TokPlus),
        ("-", TokMinus),
        ("*", TokStar),
        ("^", TokXor)
    ]


atStartOf :: String -> [(String, TokenType)] -> Maybe (String, TokenType)
atStartOf xs = find predicate
    where predicate = (`isPrefixOf` xs) . fst