module Main where

import Tokeniser (scanTokens)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let toks = scanTokens contents
    mapM_ print toks
