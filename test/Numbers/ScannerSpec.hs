module Numbers.ScannerSpec where

import Test.Hspec
import CursoredString (CursoredString)
import qualified CursoredString as CursString
import Details.Numbers.Scanner
import PPLiteral
import Control.Monad.State.Lazy

spec :: Spec
spec =
    describe "scanning numbers" $ do
        numberTest "0b1" (PPInt 1) 
        numberTest "0b1'1" (PPInt 3) 
        numberTest "0b001'000_somePrefix" (PPInt 8)
        numberTest "0b0" (PPInt 0)

main :: IO ()
main = hspec spec


numberFrom :: String -> PPLiteral 
numberFrom xs = evalState scanNumber (CursString.newCursoredString xs)

numberTest :: [Char] -> PPLiteral -> SpecWith ()
numberTest xs lit = context ("with " ++ xs) (it ("should return " ++ show lit) (numberFrom xs `shouldBe` lit))