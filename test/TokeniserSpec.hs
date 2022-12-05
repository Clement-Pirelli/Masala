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
import Control.Monad.State.Lazy

spec :: Spec
spec =
    describe "scanning tokens" $ do
        context "with short input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserShortInput
        context "with long input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserLongInput
        testDirectiveTokens "/*\nHELLO_WORLD 1\n*/" []
        testDirectiveTokens "((a+b)*c) < 5" [TokOpeningParens, TokOpeningParens, TokName, TokPlus, TokName, TokClosingParens, TokStar, TokName, TokClosingParens, TokOpeningChevron, TokLiteral]
        testDirectiveTokens "HELLO_WORLD /*1*/" [TokName]
        testDirectiveTokens "HELLO_WORLD //1" [TokName]
        testDirectiveTokens "//HELLO_WORLD 1" []
        testTokens "#include <a.h>" [TokInclude, TokChevronPath, TokEOF]
        testTokens "#include \"a.h\"" [TokInclude, TokLiteral, TokEOF]
        testTokens tokeniserShortInput shortInputTypes

main :: IO ()
main = hspec spec

tokenWithIncorrectStart :: Token -> String -> Maybe (Token, String)
tokenWithIncorrectStart tok str = if lexm `isPrefixOf` str' then Nothing else Just (tok, take 10 str')
    where
        lexm = lexeme tok
        str' = drop charOffset str
        charOffset = character (cursor tok)

allHaveCorrectStart :: String -> Expectation
allHaveCorrectStart str = catMaybes incorrectTokens `shouldBe` []
    where
        incorrectTokens = f toks
        f = map (`tokenWithIncorrectStart` str)
        toks = scanTokens str

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

shortInputTypes :: [TokenType]
shortInputTypes = [
    TokInclude, TokLiteral, 
    TokInclude, TokChevronPath,
    TokInclude, TokChevronPath,
    TokDefine, TokName, TokOpeningParens, TokName, TokClosingParens,
    TokName, TokMinus, TokLiteral,
    TokIfdef, TokName,
    TokElse,
    TokEndif,
    TokEOF
    ]

tokDescription toks
            | null toks = "shouldn't have any tokens"
            | otherwise = "should have tokens " ++ show toks

withInput :: [Char] -> [Char]
withInput input = "with body \"" ++ input ++ "\"" 