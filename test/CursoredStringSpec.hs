module CursoredStringSpec where

import Test.Hspec
import CursoredString
import TestInputs
import SpecUtils

spec :: Spec
spec =
    describe "CursoredString" $ do
        describe "asScannableString" $ do
            let scannableStringTestMapping i = (input i, expectedString i)
            testAll2 scannableStringTest $ [
                ("#def\\\r\nine A", "#define A"),
                ("#def\\\t\t\r\nine B", "#define B"),
                ("#define A\r\n#define B", "#define A\n#define B"),
                ("//#define A\r\n#define B", "#define B"),
                ("#define A/*\r\n#define B*/", "#define A"),
                ("Hello/*Bwabwa*/ World", "Hello World"),
                ("Hello//Bwabwa\n World", "Hello World")] ++ map scannableStringTestMapping inputs
        describe "advanceChars" $ do
            testAll3 advanceCharsTest [
                ("Hello World", 1, "ello World"), 
                ("Hello\\\r\nWorld", 1, "elloWorld"),
                ("Hello\\\r\nWorld", 6, "orld"),
                ("Hello// \nWorld", 5, "World"),
                ("Hello/* \n*/World", 5, "World")]
            testAll3 advanceCharsSpaceTest [
                ("Hello", 1, False),
                ("", 1, False),
                ("Hi there!", 3, True),
                ("Hello\\\r\n World", 6, True)]

main :: IO ()
main = hspec spec

scannableStringTest :: String -> String -> SpecWith ()
scannableStringTest input expected =
    context ("When getting a cursored string with contents " ++ show input) $
        it ("should return " ++ expected) $ 
            output `shouldBe` expected
    where
        output = asScannableString (newCursoredString input)

advanceCharsTest :: String -> Int -> String -> SpecWith ()
advanceCharsTest inputStr inputOffset expected =
    context ("With contents " ++ show inputStr ++ " and offset " ++ show inputOffset) $ 
        it ("should return " ++ expected) $
            asScannableString output `shouldBe` expected
    where output = advanceChars (newCursoredString inputStr) inputOffset

advanceCharsSpaceTest :: String -> Int -> Bool -> SpecWith ()
advanceCharsSpaceTest inputStr inputOffset expected =
    context ("With contents " ++ show inputStr ++ " and offset " ++ show inputOffset) $ 
        it description $
            spaceBefore output `shouldBe` expected
    where 
        output = advanceChars (newCursoredString inputStr) inputOffset
        description = (if expected then "should" else "shouldn't") ++ " have a space before"

shortInputAsScannableString = 
       "#include \"myOtherPath.h\"\n"
    ++ "#include <stddef.h>\n"
    ++ "#include <iostream>\n"
    ++ "\n"
    ++ "#define B(a)     a-1\n"
    ++ "\n"
    ++ "int main()\n"
    ++ "{\n"
    ++ "#ifdef A\n"
    ++ "    std::cout << B(0) << '\\n';\n"
    ++ "#else\n"
    ++ "    std::cout << \"A is not defined!\";\n"
    ++ "#endif\n"
    ++ "}\n"