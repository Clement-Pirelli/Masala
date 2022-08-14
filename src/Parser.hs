module Parser where

import Node
import CursoredTokens
import TokenType
import Token

parseDirective :: CursoredTokens -> (CursoredTokens, [Node])
parseDirective cts 
    | noMore cts = (cts, [])
    | otherwise = case tokenType tok of
            TokIf -> parseIf cts
            TokElif -> undefined
            TokIfdef -> undefined
            TokElse -> undefined
            TokEndif -> undefined
            TokDefine -> undefined
            TokUndef -> undefined
            TokPragma -> undefined
            TokInclude -> undefined
            TokEOF -> (cts, [])
            _ -> error $ "unexpected token: " ++ show tok ++ " Expected a directive instead"
    where
        tok = head toks
        toks = contents cts



parseIf :: CursoredTokens -> (CursoredTokens, [Node])
parseIf = undefined


onEOF lastTok = error $ "Unexpected EOF after token " ++ show lastTok