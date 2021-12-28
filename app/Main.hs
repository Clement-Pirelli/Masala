module Main where

import Tokeniser (scanTokens)

main :: IO ()
main = do
    let toks = scanTokens "#include <AAAA.h\\\n>"
    mapM_ print toks
