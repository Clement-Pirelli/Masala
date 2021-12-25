module Main where

import qualified Tokeniser as Tok

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print (show $ Tok.tokenise contents)
