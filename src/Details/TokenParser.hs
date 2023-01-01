module Details.TokenParser where

import Details.Parser
import Node
import Token

parseDirective :: Parser Node
parseDirective = parseDefine `or'` parseInclude `or'` parseIf

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

parseDefineBody :: Parser (Either [Token] [Node])
parseDefineBody = undefined

parsePPBody :: Parser Node
parsePPBody = undefined

parseIf :: Parser Node
parseIf = do
    (tok, expr) <- parseIfOrdinary `or'` parseIfDefined
    ifBody <- many' parseDirective
    elseC <- parseElse `or'` parseElseIf
    return $ Node tok If { expression = expr, body = ifBody, elseClause = elseC }
    where
        parseIfOrdinary = do
            tok <- ofType TokIf
            ifExpr <- parsePPBody
            return (tok, ifExpr)
        parseIfDefined = do
            tok <- ofType TokIfdef `or'` ofType TokIfndef
            name <- nameToSymbol
            return (tok, name)

parseElse :: Parser (Maybe Node)
parseElse = optional $ do
    tok <- ofType TokElse
    elseBody <- many' parseDirective
    _ <- ofType TokEndif
    return $ Node tok Else { body = elseBody }

parseElseIf :: Parser (Maybe Node)
parseElseIf = optional $ do
    (tok, expr) <- parseElif `or'` parseElifdef
    elseBody <- many' parseDirective
    _ <- ofType TokEndif
    return $ Node tok ElseIf { expression = expr, body = elseBody }
    where
        parseElif = do
            tok <- ofType TokElif
            expr <- parsePPBody
            return (tok, expr)
        parseElifdef = do
            tok <- ofType TokElifdef `or'` ofType TokElifndef
            expr <- nameToSymbol
            return (tok, expr)

parseInclude :: Parser Node
parseInclude = do
    tok <- ofType TokInclude
    content <- includeQuotedString `or'` includeChevronString
    return $ Node tok content

includeQuotedString :: Parser NodeContents
includeQuotedString = do
    tok <- ofType TokLiteral
    literal' <- extractLiteral tok
    content <- extractContent literal'
    return $ Include { path = content, form = QuotedInclude }
    where
        extractLiteral (Token TokLiteral _ (Just lit) _ _) = return lit
        extractLiteral _ = oops "Unexpected token. Expected string literal after #include directive"
        extractContent (PPString content StrOrdinary False) = return content
        extractContent _ = oops "Expected an ordinary string after #include directive"

includeChevronString :: Parser NodeContents
includeChevronString = do
    tok <- ofType TokChevronPath
    let asString = lexeme tok
    return $ Include { path = asString, form = ChevronInclude }

ofType :: TokenType -> Parser Token
ofType t = satisfy err ((== t) . tokenType)
    where err = "Unexpected token while trying to match token of type " ++ show t

notOfType :: TokenType -> Parser Token
notOfType t = exclude err ((== t) . tokenType)
    where err = "Unexpected token while trying to avoid tokens of type " ++ show t

noSpaceBefore :: Parser Token
noSpaceBefore = exclude "Expected no space before token" Token.preceededBySpace

nameToSymbol :: Parser Node
nameToSymbol = do 
    tok <- tname
    return $ Node tok Symbol

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