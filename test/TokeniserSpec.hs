module TokeniserSpec where

import Test.Hspec
import qualified Details.Tokeniser as DTok
import Tokeniser
import Token
import TextCursor
import Data.List (isPrefixOf)
import CursoredString (newCursoredString)
import Data.Maybe (catMaybes)

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
bodyTokenTypes input = toTypes (snd output)
    where output = DTok.scanDirectiveBody (newCursoredString input) False

testTokens input toks = 
    context (withInput input) $ 
        it tokDescription $ 
            bodyTokenTypes input `shouldBe` toks
    where
        tokDescription 
            | null toks = "shouldn't have any tokens"
            | otherwise = "should have tokens " ++ show toks
        withInput input = "with body \"" ++ input ++ "\"" 

spec :: Spec
spec =
    describe "scanning tokens" $ do
        context "with short input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserShortInput
        context "with long input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserLongInput
        testTokens "((a+b)*c) < 5" [TokOpeningParens, TokOpeningParens, TokName, TokPlus, TokName, TokClosingParens, TokStar, TokName, TokClosingParens, TokOpeningChevron, TokLiteral]
        testTokens "/*\nHELLO_WORLD 1\n*/" []
        testTokens "HELLO_WORLD /*1*/" [TokName]
        testTokens "HELLO_WORLD //1" [TokName]
        testTokens "//HELLO_WORLD 1" []

main :: IO ()
main = hspec spec

tokeniserShortInput :: String
tokeniserShortInput = "#include \"myOtherPath.h\"\n"
    ++ "#include <stddef.h>\n"
    ++ "#include <iostream>\n"
    ++ "\n"
    ++ "//#define A\n"
    ++ "#define B(a) \\\n"
    ++ "    a-1\n"
    ++ "\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "#ifdef A\n"
    ++ "    std::cout << B(0) << '\\n';\n"
    ++ "#else\n"
    ++ "    std::cout << \"A is not defined!\";\n"
    ++ "#endif\n"    ++ "}\n"

tokeniserLongInput :: String
tokeniserLongInput =
    "#include <iostream>\n"
    ++ "#include \"myPath.h\"\n"
    ++ "\n"
    ++ "#define A ((1+2-3) / 4) < 10\n"
    ++ "#ifdef MY_DEFINE\n"
    ++ "\n"
    ++ "//stuff\n"
    ++ "//stuff\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "    stuff;\n"
    ++ "    return 0;\n"
    ++ "}\n"
    ++ "\n"
    ++ "#else\n"
    ++ "\n"
    ++ "int main(\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    int argc, char **argv\n"
    ++ "#endif\n"
    ++ ")\n"
    ++ "{\n"
    ++ "#if defined(MY_OTHER_DEFINE)\n"
    ++ "    for(int i = 0; i < argc; i++)\n"
    ++ "    {\n"
    ++ "        std::cout << argv[i];\n"
    ++ "    }\n"
    ++ "    return 0;\n"
    ++ "#else\n"
    ++ "    return 1;\n"
    ++ "#endif\n"
    ++ "}\n"
    ++ "//stuff\n"
    ++ "\n"
    ++ "#endif\n"
