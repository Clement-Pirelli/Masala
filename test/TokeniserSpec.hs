module TokeniserSpec where

import Test.Hspec
import qualified Details.Tokeniser as DTok 
import Tokeniser
import Token
import CursorPosition
import TestData.TokeniserInput
import Data.List (isPrefixOf)
import CursoredString (newCursoredString)

hasCorrectStart :: Token -> String -> Bool
hasCorrectStart tok str = lexm `isPrefixOf` str'
    where
        lexm = lexeme tok
        str' = drop charOffset str
        charOffset = character (cursor tok)

allHaveCorrectStart :: String -> Bool
allHaveCorrectStart str = f toks
    where
        f = all (`hasCorrectStart` str)
        toks = scanTokens str

toTypes :: [Token] -> [TokenType]
toTypes = map tokenType

bodyTokenTypes :: String -> [TokenType]
bodyTokenTypes input = toTypes (snd output)
    where output = DTok.scanDirectiveBody (newCursoredString input) False

spec :: Spec
spec =
    describe "scanning tokens" $ do
        context "with small input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserSmallInput
        context "with long input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserLongInput
        context "with body \"((a+b)*c) < 5\"" $
            it "should have tokens (, (, +, ), *, ), <" $ --todo: add symbol tokens when we can parse symbols
                bodyTokenTypes "((a+b)*c) < 5" `shouldBe` [TokOpeningParens, TokOpeningParens, TokPlus, TokClosingParens, TokStar, TokClosingParens, TokOpeningChevron]

main :: IO ()
main = hspec spec