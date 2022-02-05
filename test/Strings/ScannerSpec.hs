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

shouldScanToLit :: String -> PPLiteral -> SpecWith ()
shouldScanToLit input expected = 
    context ("with " ++ show input) $
        it ("should scan to " ++ show expected) $
            stringFrom input `shouldBe` expected

makeOrdinary :: String -> PPLiteral
makeOrdinary xs = (PPString {ppstrContents=xs, ppstrType=StrOrdinary, ppstrRaw=False})

shouldScanTo :: String -> String -> SpecWith ()
shouldScanTo input expected = shouldScanToLit input (makeOrdinary expected)

shouldLeave :: String -> String -> SpecWith ()
shouldLeave input expected = context ("with " ++ show input) $
                                    it ("should leave " ++ show expected) $
                                        (CursString.asScannableString . fst . scan) input `shouldBe` expected

literal :: String -> String
literal xs = '\"':xs++"\""

raw :: String -> String
raw xs = 'R':xs

withPrefix :: String -> String -> String
withPrefix xs prefix = prefix ++ xs

spec :: Spec
spec =
    describe "scanning strings" $ do
        literal "Hello World!" `shouldScanTo` "Hello World!"
        literal "cstdint.h" `shouldScanTo` "cstdint.h"
        literal "Hello\\r\\nWorld" `shouldScanTo` "Hello\r\nWorld"
        literal "Hello\\\n\\r\\nWorld" `shouldScanTo` "Hello\r\nWorld"
        literal "Hello\\tWorld" `shouldScanTo` "Hello\tWorld"
        ((`withPrefix` "u8") . raw . literal) "foo(bar\\nbar)foo" `shouldScanToLit` PPString {ppstrContents="bar\\nbar", ppstrType=StrUTF8, ppstrRaw=True}
        ((`withPrefix` "L") . literal) "The quick brown fox jumps over the lazy dog" `shouldScanToLit` PPString {ppstrContents="The quick brown fox jumps over the lazy dog", ppstrType=StrLong, ppstrRaw=False}
        literal "\\0" `shouldScanTo` "\0"

        literal "Hello World!" `shouldLeave` ""
        "\"Hello World!\"\nHow are you" `shouldLeave` "\nHow are you"

main :: IO ()
main = hspec spec