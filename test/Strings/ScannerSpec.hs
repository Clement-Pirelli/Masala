module Strings.ScannerSpec where

import Test.Hspec
import CursorPosition
import CursoredString (CursoredString)
import qualified CursoredString as CursString
import Details.Strings.Scanner
import PPLiteral

scan :: String -> (CursoredString, PPLiteral)
scan xs = scanString $ CursString.newCursoredString xs

stringFrom :: String -> PPLiteral 
stringFrom = snd . scan

shouldScanTo :: String -> String -> SpecWith ()
shouldScanTo input expected = context ("with " ++ show input) $ 
                                    it ("should scan to " ++ show expected) $ 
                                        stringFrom input `shouldBe` lit
    where lit = PPString expected StrOrdinary False

shouldLeave :: String -> String -> SpecWith ()
shouldLeave input expected = context ("with " ++ show input) $ 
                                    it ("should leave " ++ show expected) $ 
                                        (CursString.asScannableString . fst . scan) input `shouldBe` expected 

makeOrdinary :: String -> String
makeOrdinary xs = '\"':xs++"\""

spec :: Spec
spec =
    describe "scanning strings" $ do
        makeOrdinary "Hello World!" `shouldScanTo` "Hello World!"
        makeOrdinary "cstdint.h" `shouldScanTo` "cstdint.h"
        makeOrdinary "Hello\\r\\nWorld" `shouldScanTo` "Hello\r\nWorld"
        makeOrdinary "Hello\\\n\\r\\nWorld" `shouldScanTo` "Hello\r\nWorld"
        makeOrdinary "Hello\\tWorld" `shouldScanTo` "Hello\tWorld"

        makeOrdinary "Hello World!" `shouldLeave` ""
        "\"Hello World!\"\nHow are you" `shouldLeave` "\nHow are you"

main :: IO ()
main = hspec spec