module CursoredStringSpec where

import Test.Hspec
import CursoredString


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

spec :: Spec
spec =
    describe "CursoredString" $ do
        describe "asScannableString" $ do
            scannableStringTest "#def\\\r\nine A" "#define A"
            scannableStringTest "#def\\\t\t\r\nine B" "#define B"
            scannableStringTest "#define A\r\n#define B" "#define A\n#define B"
        describe "advanceChars" $ do
            advanceCharsTest "Hello World" 1 "ello World"
            advanceCharsTest "Hello\\\r\nWorld" 1 "elloWorld"
            advanceCharsTest "Hello\\\r\nWorld" 6 "orld"



main :: IO ()
main = hspec spec