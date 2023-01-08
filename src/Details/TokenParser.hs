module Details.TokenParser where

import Details.Parser
import Node
import Token

parseDirective :: Parser Node
parseDirective = parseDefine <|> parseInclude <|> parseIf

parseDefine :: Parser Node
parseDefine = do
            tok <- tdefine
            name <- nameToSymbol
            (noSpace, parameters) <- optional noSpaceBefore `and'` funcLike
            defineBody <- parseDefineBody
            let nodeParameters = noSpace >> parameters
            makeNode tok Define { symbol = name, params = nodeParameters, defineContents = defineBody }
            where
                funcLike = optional $ betweenParens funcParams
                funcParams = (nameToSymbol `separatedBy` tcomma) <|> result []

parseDefineBody :: Parser (Either [Token] [Node])
parseDefineBody = do
    nodes <- parseDefineBody'
    if null nodes then
        Left <$> tokensToNextDirective
    else
        return $ Right nodes

parseDefineBody' :: Parser [Node]
parseDefineBody' = do
    maybeBody <- optional parseIfBody
    case maybeBody of
      Nothing -> return []
      Just n -> return [n]
        

parseIfBody :: Parser Node
parseIfBody = parseSymbolOrNumber

parseSymbolOrNumber :: Parser Node
parseSymbolOrNumber = nameToSymbol <|> parseInt

parseInt :: Parser Node
parseInt = do
    tok <- ofType TokLiteral
    case literal tok of
        Just l -> case l of
            PPInt _ -> makeNode tok Literal
            _ -> oops "Unexpected type of literal, expected integer"
        _ -> error "Literal token did not have a literal. This should never happen :'("

parseIf :: Parser Node
parseIf = do
    (tok, expr) <- parseIfOrdinary <|> parseIfDefined
    ifBody <- many' parseDirective
    elseC <- parseElse <|> parseElseIf
    makeNode tok If { expression = expr, body = ifBody, elseClause = elseC }
    where
        parseIfOrdinary = do
            tok <- ofType TokIf
            ifExpr <- parseIfBody
            return (tok, ifExpr)
        parseIfDefined = do
            tok <- ofType TokIfdef <|> ofType TokIfndef
            name <- nameToSymbol
            return (tok, name)

parseElse :: Parser (Maybe Node)
parseElse = optional $ do
    tok <- ofType TokElse
    elseBody <- many' parseDirective
    _ <- ofType TokEndif
    makeNode tok Else { body = elseBody }

parseElseIf :: Parser (Maybe Node)
parseElseIf = optional $ do
    (tok, expr) <- parseElif <|> parseElifdef
    elseBody <- many' parseDirective
    _ <- ofType TokEndif
    makeNode tok ElseIf { expression = expr, body = elseBody }
    where
        parseElif = do
            tok <- ofType TokElif
            expr <- parseIfBody
            return (tok, expr)
        parseElifdef = do
            tok <- ofType TokElifdef <|> ofType TokElifndef
            expr <- nameToSymbol
            return (tok, expr)

parseInclude :: Parser Node
parseInclude = do
    tok <- ofType TokInclude
    content <- includeQuotedString <|> includeChevronString
    makeNode tok content

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

--todo: use or remove this
--notOfType :: TokenType -> Parser Token
--notOfType t = exclude err ((== t) . tokenType)
--    where err = "Unexpected token while trying to avoid tokens of type " ++ show t

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

makeNode :: Token -> NodeContents -> Parser Node
makeNode tok c = pure $ Node tok c

betweenParens :: Parser b -> Parser b
betweenParens parser = surroundedBy (ofType TokOpeningParens) parser (ofType TokClosingParens)

tokensToNextDirective :: Parser [Token]
tokensToNextDirective = zeroOrMore $ satisfy "" notDirective --error message will not be used here, zeroOrMore will return an empty list on failure
    where
        notDirective tok = tokenType tok `notElem` directiveTypes 
        directiveTypes = TokEOF : map snd directiveTokens