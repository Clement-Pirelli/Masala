module Details.TokenParser where

import Details.Parser
import Node
import Token
import Cursored(peek)

parseDirective :: Parser Node
parseDirective = parseDefine `or'` parseInclude

parseDefine :: Parser Node
parseDefine = do
            (tok, name) <- tdefine `followedBy` nameToSymbol
            parameters <- funcLike
            return $ Node tok Define { symbol = name, params = parameters, defineContents = Right [] }
            where
                funcLike = optional $ betweenParens ((nameToSymbol `separatedBy` tcomma) `or'` result [])
                objectLike = optional spaceBefore 

parseInclude :: Parser Node
parseInclude = do
    tok <- ofType TokInclude
    (strTok, str) <- ofType TokLiteral `and'` includeString
    let parsed = Node tok $ Include { path = str, form = Quoted }
    return parsed

includeString :: Parser String
includeString = do
    tok <- item
    let notStrErr = "Unexpected token. Expected string literal after #include directive"
    literal <- extractLiteral notStrErr tok
    extractContents "Expected an ordinary string after #include directive" literal
    where
        extractLiteral _ (Token TokLiteral _ (Just lit) _ _) = return lit
        extractLiteral err _ = oops err 
        extractContents _ (PPString contents StrOrdinary False) = return contents
        extractContents err _ = oops err 

ofType :: TokenType -> Parser Token
ofType t = satisfy err ((== t) . tokenType)
    where err = "Unexpected token while trying to match token of type " ++ show t

spaceBefore :: Parser Token
spaceBefore = satisfy "Expected a space before token" Token.preceededBySpace

nameToSymbol :: Parser Node
nameToSymbol = Parser $ \curs-> do 
    (name, next) <- parse tname curs
    let symbol = Node name Symbol
    return (symbol, next)

tname :: Parser Token
tname = ofType TokName

tcomma :: Parser Token
tcomma = ofType TokComma

tdefine :: Parser Token
tdefine = ofType TokDefine

betweenParens :: Parser b -> Parser b
betweenParens x = surroundedBy (ofType TokOpeningParens) x (ofType TokClosingParens)