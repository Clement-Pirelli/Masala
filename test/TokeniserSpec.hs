module TokeniserSpec where

import Test.Hspec
import qualified Details.Tokeniser as DTok
import Tokeniser
import Token
import TextCursor
import Data.List (isPrefixOf)
import CursoredString (newCursoredString)
import Data.Maybe (catMaybes)
import TestInputs
import DebugUtils
import SpecUtils
import Control.Monad.State.Lazy

spec :: Spec
spec =
    describe "scanning tokens" $ do
        let equationExample = ("((a+b)*c) < 5", [TokOpeningParens, TokOpeningParens, TokName, TokPlus, TokName, TokClosingParens, TokStar, TokName, TokClosingParens, TokOpeningChevron, TokLiteral])
        testAll2 testDirectiveTokens [
            ("/*\nHELLO_WORLD 1\n*/", []), 
            equationExample, 
            ("HELLO_WORLD /*1*/", [TokName]), 
            ("HELLO_WORLD //1", [TokName]),
            ("//HELLO_WORLD 1", [])]
        let testTokensMapping i = (input i, expectedTypes i)
        testAll2 testTokens $ [
            ("#include <a.h>", [TokInclude, TokChevronPath, TokEOF]),
            ("#include \"a.h\"", [TokInclude, TokLiteral, TokEOF])] 
            ++ map testTokensMapping inputs

main :: IO ()
main = hspec spec

toTypes :: [Token] -> [TokenType]
toTypes = map tokenType

bodyTokenTypes :: String -> [TokenType]
bodyTokenTypes input = toTypes output
    where output = evalState DTok.scanDirectiveBody (newCursoredString input) 

tokenTypes :: String -> [TokenType]
tokenTypes = toTypes . scanTokens

testDirectiveTokens :: [Char] -> [TokenType] -> SpecWith ()
testDirectiveTokens input toks = 
    context (withInput input) $ 
        it (tokDescription toks) $ 
            bodyTokenTypes input `shouldBe` toks

testTokens :: [Char] -> [TokenType] -> SpecWith ()
testTokens input toks = 
    context (withInput input) $ 
        it (tokDescription toks) $ 
            tokenTypes input `shouldBe` toks

--details

tokDescription toks
            | null toks = "shouldn't have any tokens"
            | otherwise = "should have tokens " ++ show toks

withInput :: [Char] -> [Char]
withInput input = "with body \"" ++ input ++ "\"" 