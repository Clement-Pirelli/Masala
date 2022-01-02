module TokeniserSpec where

import Test.Hspec
import qualified Details.Tokeniser as DTok 
import Tokeniser
import Token
import CursorPosition
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
        context "with short input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserShortInput
        context "with long input" $
            it "should all have correct char number" $
                allHaveCorrectStart tokeniserLongInput
        context "with body \"((a+b)*c) < 5\"" $
            it "should have tokens (, (, +, ), *, ), <" $ --todo: add symbol tokens when we can parse symbols
                bodyTokenTypes "((a+b)*c) < 5" `shouldBe` [TokOpeningParens, TokOpeningParens, TokPlus, TokClosingParens, TokStar, TokClosingParens, TokOpeningChevron]

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
    ++ "#endif\n"
    ++ "}\n"

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