module Details.TokenParser where

import Details.Parser
import Node
import Token

parseDirective :: Parser Node
parseDirective = parseDefine `or'` parseInclude

parseDefine :: Parser Node
parseDefine = do
            tok <- tdefine
            name <- nameToSymbol
            (noSpace, parameters) <- optional noSpaceBefore `and'` funcLike
            let nodeParameters = noSpace >> parameters
            return $ Node tok Define { symbol = name, params = nodeParameters, defineContents = Right [] }
            where
                funcLike = optional $ betweenParens funcParams
                funcParams = (nameToSymbol `separatedBy` tcomma) `or'` result []

parseInclude :: Parser Node
parseInclude = do
    tok <- ofType TokInclude
    content <- includeQuotedString `or'` includeChevronString
    let parsed = Node tok content
    return parsed

includeQuotedString :: Parser NodeContents
includeQuotedString = do
    tok <- ofType TokLiteral
    let notStrErr = "Unexpected token. Expected string literal after #include directive"
    literal' <- extractLiteral notStrErr tok
    content <- extractContent "Expected an ordinary string after #include directive" literal'
    return $ Include { path = content, form = QuotedInclude }
    where
        extractLiteral _ (Token TokLiteral _ (Just lit) _ _) = return lit
        extractLiteral err _ = oops err
        extractContent _ (PPString content StrOrdinary False) = return content
        extractContent err _ = oops err

includeChevronString :: Parser NodeContents
includeChevronString = do
    let
        open = ofType TokOpeningChevron
        close = ofType TokClosingChevron
    toks <- surroundedBy open (many' middle) close
    let asString = concatMap lexeme toks
    return $ Include { path = asString, form = ChevronInclude }
    where
        middle = validateWithThen notEOF (notOfType TokClosingChevron)

ofType :: TokenType -> Parser Token
ofType t = satisfy err ((== t) . tokenType)
    where err = "Unexpected token while trying to match token of type " ++ show t

notOfType :: TokenType -> Parser Token
notOfType t = exclude err ((== t) . tokenType)
    where err = "Unexpected token while trying to avoid tokens of type " ++ show t

noSpaceBefore :: Parser Token
noSpaceBefore = exclude "Expected no space before token" Token.preceededBySpace

nameToSymbol :: Parser Node
nameToSymbol = Parser $ \curs-> do 
    (name, next) <- parse tname curs
    let symbol' = Node name Symbol
    return (symbol', next)

tname :: Parser Token
tname = ofType TokName

tcomma :: Parser Token
tcomma = ofType TokComma

tdefine :: Parser Token
tdefine = ofType TokDefine

betweenParens :: Parser b -> Parser b
betweenParens parser = surroundedBy (ofType TokOpeningParens) parser (ofType TokClosingParens)

notEOF :: Parser Token
notEOF = exclude "Unexpected end of file" isEOF
    where
        isEOF t = tokenType t == TokEOF