module NumberScannerSpec where

import SpecHelper
import CursorPosition
import CursoredString (CursoredString)
import qualified CursoredString as CursString
import Details.NumberScanner
import PPLiteral

numberFrom :: String -> PPLiteral 
numberFrom xs = snd (scanNumber $ CursString.newCursoredString xs)

numberTest :: [Char] -> PPLiteral -> SpecWith ()
numberTest xs lit = context ("with " ++ xs) (it ("should return " ++ show lit) (numberFrom xs `shouldBe` lit))

spec :: Spec
spec =
    describe "scanning numbers" $ do
        numberTest "0b1" (PPInt 1) 
        numberTest "0b1'1" (PPInt 3) 
        numberTest "0b001'000_somePrefix" (PPInt 8)
        numberTest "0b0" (PPInt 0)

main :: IO ()
main = hspec spec