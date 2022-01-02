module Details.Numbers.Scanner where

import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Details.Numbers.Base
import Data.Char (toLower, digitToInt, intToDigit, isDigit, toUpper)
import Data.List (isPrefixOf, foldl')
import Details.Strings.Utils(startsWith)

scannableAsNumberLiteral :: String -> Bool
scannableAsNumberLiteral _ = False -- todo

scanNumber :: CursoredString -> (CursoredString, PPLiteral)
scanNumber cs --todo: fix this
    | isFloatingPoint = scanFloatingPoint cs
    | startsWith "0b" = scanBinary $ CursString.advanceChars cs 2
    | startsWith "0x" = scanHexa $ CursString.advanceChars cs 2
    | startsWith "0" = scanOctal cs -- no need to advance, and it simplifies things for error reporting on incomplete int literals
    | otherwise = scanDecimal cs
    where
        startsWith y = y `isPrefixOf` lowerxs
        lowerxs = map toLower xs
        isFloatingPoint = False --Figure out a way to do this, stat. Probably a regex again
        xs = CursString.asScannableString cs

scanBinary = scanInteger (Base 2)
scanHexa = scanInteger (Base 16)
scanOctal = scanInteger (Base 8)
scanDecimal = scanInteger (Base 10)

scanInteger :: Base -> CursoredString -> (CursoredString, PPLiteral)
scanInteger base cs = (CursString.advanceChars cs offset, PPInt n)
    where
        (offset, n) = step (0, 0) xs
        xs = CursString.asScannableString cs
        step t@(count, oN) (c:str)
            | c == '\'' = step t str
            | c `isBase` base = step (count + 1, toInteger (digitToInt c) + (oN `asBase` base)) str
            | otherwise = if count > 0 then t else error $ "Incomplete integral literal at " ++ show cs
        step t [] = t


scanFloatingPoint :: CursoredString -> (CursoredString, PPLiteral)
scanFloatingPoint = undefined