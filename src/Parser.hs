module Parser(parsePP) where

import Details.Parser
import Details.TokenParser
import Node
import Token
import Cursored

parsePP :: [Token] -> [Node]
parsePP toks = case parsed of
        Right (nodes, _) -> nodes
        Left err -> error err
    where
        parsed = parse (many' parseDirective) curs 
        curs = Cursored toks 0