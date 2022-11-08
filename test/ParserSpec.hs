module ParserSpec where

import Test.Hspec
import Details.Parser
import Parser
import Details.TokenParser
import Token
import Tokeniser
import Node
import Data.Either

shouldMatchWith :: String -> (Node -> Bool) -> String -> SpecWith ()
shouldMatchWith input predicate description =
    context ("with " ++ show input) $
        it ("the first parsed node should match with " ++ description) $
            let allMatch = fmap predicate ((parseTokens . scanTokens) input) 
            in allMatch `shouldBe` [True]

paramsMatch :: ([Node] -> Bool) -> Node -> Bool
paramsMatch f node = defineParamsAre node (maybe False f)

shouldError :: String -> SpecWith ()
shouldError input = context ("with" ++ show input) $
    it "should result in an error" $
        isError `shouldBe` True
    where
        isError = not $ isRight $ (parseTokensLenient . scanTokens) input

include :: IncludeForm -> String -> Node -> Bool
include form path node = case contents node of 
                            Include path form -> True
                            _ -> False


spec :: Spec
spec =
    describe "Parser" $ do
        describe "parsePP" $ do
            shouldMatchWith "#define A" (\n -> defineSymbolIs n (`lexemeIs` "A")) "having a define with symbol \"A\""
            shouldMatchWith "#define A() " (paramsMatch null) "having a function-like define with 0 params"
            let isLexeme = flip lexemeIs
                areRightNames = and . zipWith isLexeme ["a", "b", "c", "d"]
                hasRightParams = paramsMatch areRightNames
            shouldMatchWith "#define A(a, b, c, d) " hasRightParams "having a function-like define with params a b c and d"
            shouldMatchWith "#include \"a.h\"" (include QuotedInclude "a.h") "an ordinary include whose path is \"a.h\""
            shouldMatchWith "#include <a.h>" (include ChevronInclude "a.h") "a chevron include whose path is \"a.h\""
            shouldError "#include"
            shouldError "#define"

main :: IO ()
main = hspec spec