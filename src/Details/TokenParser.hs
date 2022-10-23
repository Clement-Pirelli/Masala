module Details.TokenParser where

import Details.Parser
import Node
import Token

parseDirective :: Parser Node
parseDirective = parseDefine

parseDefine :: Parser Node
parseDefine = declaration
    where
        declaration = Parser $ \toks -> do
            ((tok, name), afterDefine) <- parse (tdefine `followedBy` nameToSymbol) toks
            (parameters, afterParams) <- parse funcLike afterDefine
            let parsed = Node tok Define { symbol = name, params = parameters, defineContents = Right [] }
            return (parsed, afterParams)
        funcLike = optional $ betweenParens ((nameToSymbol `separatedBy` tcomma) `or'` result [])
        objectLike = optional spaceBefore 

ofType :: TokenType -> Parser Token
ofType t = satisfy err ((== t) . tokenType)
    where err x = "Unexpected token '" ++ show x ++ "' while trying to match token of type " ++ show t

spaceBefore :: Parser Token
spaceBefore = satisfy (\t -> "Expected a space before token " ++ show t) Token.preceededBySpace

tdefine :: Parser Token
tdefine = ofType TokDefine

nameToSymbol :: Parser Node
nameToSymbol = Parser $ \curs-> do 
    (name, next) <- parse tname curs
    let symbol = Node name Symbol
    return (symbol, next)

tname :: Parser Token
tname = ofType TokName

tcomma :: Parser Token
tcomma = ofType TokComma

betweenParens :: Parser b -> Parser b
betweenParens x = surroundedBy (ofType TokOpeningParens) x (ofType TokClosingParens)