module Token(module Token, module TokenType, module PPLiteral) where

import TokenType
import PPLiteral
import TextCursor(TextCursor)

import Data.List(isPrefixOf, find)

data Token = Token {
      tokenType :: TokenType
    , lexeme :: String
    , literal :: Maybe PPLiteral
    , cursor :: TextCursor 
    , preceededBySpace :: Bool
    } deriving (Show, Eq)

withLiteral :: Token -> PPLiteral -> Token
withLiteral (Token tokType lexm _ curs space) lit = Token tokType lexm (Just lit) curs space

directiveTokens :: [(String, TokenType)]
directiveTokens = [
        ("include", TokInclude),
        ("define", TokDefine),
        ("pragma", TokPragma),
        ("ifdef", TokIfdef),
        ("ifndef", TokIfndef),
        ("endif", TokEndif),
        ("elifdef", TokElifdef),
        ("elifndef", TokElifndef),
        ("elif", TokElif),
        ("else", TokElse),
        ("if", TokIf),
        ("undef", TokUndef)
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