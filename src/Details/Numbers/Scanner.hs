module Details.Numbers.Scanner where

import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Details.Numbers.Base
import Data.Char (toLower, digitToInt, isNumber)
import Data.List (isPrefixOf)
import Text.Regex.TDFA

scannableAsFloatingPoint :: String -> Bool
scannableAsFloatingPoint xs = xs =~ decimal
    where
        decimal = "(\\d*\\.)" --Todo: can't really use regex here

scannableAsIntegral :: String -> Bool
scannableAsIntegral [] = False
scannableAsIntegral (x:_) = isNumber x

scanNumber :: CursoredString -> (CursoredString, PPLiteral)
scanNumber cs --todo: fix this
    | isFloatingPoint = scanFloatingPoint cs
    | startsWith' "0b" = scanBinary $ CursString.advanceChars cs 2
    | startsWith' "0x" = scanHexa $ CursString.advanceChars cs 2
    | startsWith' "0" = scanOctal cs -- no need to advance, and it simplifies things for error reporting on incomplete int literals
    | otherwise = scanDecimal cs
    where
        startsWith' y = y `isPrefixOf` lowerxs
        lowerxs = map toLower xs
        isFloatingPoint = False --Figure out a way to do this, stat. Probably a regex again
        xs = CursString.asScannableString cs

scanBinary :: CursoredString -> (CursoredString, PPLiteral)
scanBinary cs = scanInteger ( IntegerScan (Base 2) cs ['\''])
scanHexa :: CursoredString -> (CursoredString, PPLiteral)
scanHexa cs = scanInteger ( IntegerScan (Base 16) cs ['\''])
scanOctal :: CursoredString -> (CursoredString, PPLiteral)
scanOctal cs = scanInteger ( IntegerScan (Base 8) cs ['\''])
scanDecimal :: CursoredString -> (CursoredString, PPLiteral)
scanDecimal cs = scanInteger ( IntegerScan (Base 10) cs ['\''])


data IntegerScan = IntegerScan Base CursoredString [Char]

scanInteger :: IntegerScan -> (CursoredString, PPLiteral)
scanInteger (IntegerScan base scanned ignore) = (CursString.advanceChars scanned offset, PPInt n)
    where
        (offset, n) = step (0, 0) xs
        xs = CursString.asScannableString scanned
        step t@(count, oN) (c:str)
            | c `elem` ignore = step t str
            | c `isBase` base = step (count + 1, toInteger (digitToInt c) + (oN `asBase` base)) str
            | otherwise = if count > 0 then t else error $ "Incomplete integral literal at " ++ show scanned
        step t [] = t



scanFloatingPoint :: CursoredString -> (CursoredString, PPLiteral)
scanFloatingPoint = undefined