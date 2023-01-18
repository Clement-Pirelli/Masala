module Details.TokenParser where

import Details.Parser
import Node
import Token
import Data.Maybe (maybeToList)

parseDirective :: Parser Node
parseDirective = parseDefine <|> parseInclude <|> parseIf <|> parseUndef <|> oops "Ran out of parsers "

parseDefine :: Parser Node
parseDefine = do
    tok <- tdefine
    name <- nameToSymbol
    parameters <- optional parseFuncParams
    defineBody <- parseDefineBody
    makeNode tok Define { symbol = name, params = parameters, defineContents = defineBody }

parseDefineBody :: Parser (Either [Token] Node)
parseDefineBody = do
    expr <- optional parseExpr
    case expr of 
      Nothing -> Left <$> tokensToNextDirective
      Just e -> return $ Right e

parseExpr :: Parser Node
parseExpr = parseBinaryOp <|> parseAtomicExpr 

parseAtomicExpr :: Parser Node
parseAtomicExpr = parseUnaryOp <|> parseSimpleExpr

parseSimpleExpr :: Parser Node
parseSimpleExpr = parseInt <|> nameToSymbol <|> parseFuncApplication <|> betweenParens parseExpr

parseFuncApplication :: Parser Node
parseFuncApplication = do
    tok <- ofType TokName
    ops <- parseFuncParams
    makeNode tok FuncLikeApplication { operands = ops }

parseUnaryOp :: Parser Node
parseUnaryOp = do
    (x, opType) <- firstOf "Unrecognized unary operator" (fmap parseOperator tokensToUnaryOps) --just linear search should be fine
    op <- parseSimpleExpr
    makeNode x UnaryOp {operand = op, unaryOpType = opType}
    where
        parseOperator (t, op) = do
            x <- ofType t
            return (x, op)

parseBinaryOp :: Parser Node
parseBinaryOp = parseBinaryOp' tokensToBinaryOps

parseBinaryOp' ::  [(TokenType, BinaryOpType)] -> Parser Node
parseBinaryOp' [] = parseAtomicExpr
parseBinaryOp' ((tokType, binType):rest) = chainl1 (parseBinaryOp' rest) thisOp
    where
        thisOp = do 
            tok <- ofType tokType
            return (\l r -> Node tok BinaryOp { left = l, right = r, binaryOpType = binType } )

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
    ifBody <- zeroOrMore parseDirective
    elseIfs <- zeroOrMore parseElseIf
    elseC <- optional parseElse
    _ <- ofType TokEndif
    makeNode tok If { expression = expr, body = ifBody, elseClauses = elseIfs ++ maybeToList elseC }
    where
        parseIfOrdinary = do
            tok <- ofType TokIf
            ifExpr <- parseExpr
            return (tok, ifExpr)
        parseIfDefined = do
            tok <- ofType TokIfdef <|> ofType TokIfndef
            name <- nameToSymbol
            return (tok, name)

parseElse :: Parser Node
parseElse = do
    tok <- ofType TokElse
    elseBody <- zeroOrMore parseDirective
    makeNode tok Else { body = elseBody }

parseElseIf :: Parser Node
parseElseIf = do
    (tok, expr) <- parseElif <|> parseElifdef
    elseBody <- zeroOrMore parseDirective
    makeNode tok ElseIf { expression = expr, body = elseBody }
    where
        parseElif = do
            tok <- ofType TokElif
            expr <- parseExpr
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

parseUndef :: Parser Node
parseUndef = do
    tok <- ofType TokUndef
    s <- nameToSymbol
    makeNode tok Undef { symbol = s }

parseFuncParams :: Parser [Node]
parseFuncParams = validateWithThen noSpaceBefore ps
    where ps = betweenParens $ (nameToSymbol `separatedBy` tcomma) <|> result []


ofType :: TokenType -> Parser Token
ofType t = satisfy err ((== t) . tokenType)
    where err = "Unexpected token while trying to match token of type " ++ show t

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