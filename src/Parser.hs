module Parser(parseTokens, parseTokensLenient) where

import Details.Parser
import Details.TokenParser
import Node
import Token
import Cursored
import Details.EitherUtils
import Data.Maybe

parseTokens :: [Token] -> [Node]
parseTokens = unwrapEither . parseTokensLenient

parseTokensLenient :: [Token] -> Either String [Node]
parseTokensLenient toks = fmap fst parsed
    where
        parsed = parse parser curs 
        curs = Cursored toks 0

parser :: Parser [Node]
parser = do 
    eof <- optional $ peeked $ ofType TokEOF
    if isJust eof
        then return []
        else do
            dir <- parseDirective
            dirs <- parser
            return $ dir : dirs 
    