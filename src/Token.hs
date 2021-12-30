module Token(module Token, module TokenType, module PPLiteral) where

import TokenType
import PPLiteral
import CursorPosition(CursorPosition)

import Data.List(isPrefixOf, find)

data Token = Token {
      tokenType :: TokenType
    , lexeme :: String
    , literal :: Maybe PPLiteral
    , cursor :: CursorPosition 
    , preceededBySpace :: Bool
    } deriving (Show, Eq)

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
        (">=", TokGreaterEqual),
        ("<=", TokLesserEqual),
        ("==", TokIsEqual),
        ("&&", TokAnd),
        ("!=", TokNotEqual),
        ("||", TokOr),
        ("##", TokHashHash),
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
        ("/", TokSlash),
        ("^", TokXor),
        ("#", TokHash),
        ("{", TokOpeningBrace),
        ("}", TokClosingBrace),
        ("[", TokOpeningBracket),
        ("]", TokClosingBracket),
        (".", TokDot),
        ("=", TokEquals)
    ]


atStartOf :: String -> [(String, TokenType)] -> Maybe (String, TokenType)
atStartOf xs = find predicate
    where predicate = (`isPrefixOf` xs) . fst