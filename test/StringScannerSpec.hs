module StringScannerSpec where

import SpecHelper
import CursorPosition
import CursoredString (CursoredString)
import qualified CursoredString as CursString
import Details.StringScanner
import PPLiteral

stringFrom :: String -> PPLiteral 
stringFrom xs = snd (scanString $ CursString.newCursoredString xs)

stringTestOrdinary :: String -> String -> SpecWith ()
stringTestOrdinary input expected = context ("with " ++ show input) $ 
                                    it ("should return " ++ show expected) $ 
                                        stringFrom (makeOrdinary input) `shouldBe` lit
    where lit = PPString expected StrOrdinary False

makeOrdinary :: String -> String
makeOrdinary xs = '\"':xs++"\""

spec :: Spec
spec =
    describe "scanning strings" $ do
        stringTestOrdinary "Hello World!" "Hello World!"
        stringTestOrdinary "cstdint.h" "cstdint.h"
        stringTestOrdinary "Hello\\r\\nWorld" "Hello\r\nWorld"
        stringTestOrdinary "Hello\\\n\\r\\nWorld" "Hello\r\nWorld"
        stringTestOrdinary "Hello\\tWorld" "Hello\tWorld"

main :: IO ()
main = hspec spec