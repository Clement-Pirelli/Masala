module ParserSpec where

import Test.Hspec
import Details.Parser
import Parser
import Details.TokenParser
import Token
import Cursored
import Tokeniser
import Node(Node, defineSymbolIs, lexemeIs, defineParamsAre)

shouldMatchWith :: String -> (Node -> Bool) -> String -> SpecWith ()
shouldMatchWith input predicate description =
    context ("with " ++ show input) $
        it ("the first parsed node should match with " ++ description) $
            let a = fmap predicate ((parsePP . scanTokens) input) 
            in a `shouldBe` [True]

spec :: Spec
spec =
    describe "Parser" $ do
        describe "parsePP" $ do
            shouldMatchWith "#define A" (\n -> defineSymbolIs n (`lexemeIs` "A")) "having a define with symbol \"A\""
            shouldMatchWith "#define A() " (\n -> defineParamsAre n (maybe False null)) "having a function-like define with 0 params"

main :: IO ()
main = hspec spec