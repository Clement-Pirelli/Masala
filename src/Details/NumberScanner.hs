module Details.NumberScanner where

import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Data.Char (toLower, digitToInt, intToDigit, isDigit, toUpper)
import Data.List (isPrefixOf, foldl')
import ListUtils (safeHead)



scannableAsNumberLiteral :: String -> Bool
scannableAsNumberLiteral _ = False -- todo

scanNumber :: CursoredString -> (CursoredString, PPLiteral)
scanNumber cs --also todo: refactor this into regex maybe? Or a lookup perhaps
    | isFloatingPoint = scanFloatingPoint cs
    | startsWith "0b" = scanBinary $ CursString.advanceChars cs 2
    | startsWith "0x" = scanHexa $ CursString.advanceChars cs 2
    | startsWith "0" = scanOctal cs -- no need to advance, and it simplifies things for error reporting on incomplete int literals
    | otherwise = scanDecimal cs
    where
        startsWith y = y `isPrefixOf` lowerxs
        lowerxs = map toLower xs
        isFloatingPoint = (safeHead . dropWhile isDigit) xs == Just '.' --This is wrong, decimals can have 0x too and such. Figure out a better way to do this, stat
        xs = CursString.asScannableString cs

scanBinary :: CursoredString -> (CursoredString, PPLiteral)
scanBinary = scanInteger (Base 2)

scanHexa :: CursoredString -> (CursoredString, PPLiteral)
scanHexa = scanInteger (Base 16)

scanOctal :: CursoredString -> (CursoredString, PPLiteral)
scanOctal = scanInteger (Base 8)

scanDecimal :: CursoredString -> (CursoredString, PPLiteral)
scanDecimal = scanInteger (Base 10)

scanInteger :: Base -> CursoredString -> (CursoredString, PPLiteral)
scanInteger base cs = (CursString.advanceChars cs offset, PPInt n)
    where
        (offset, n) = step (0, 0) xs
        xs = CursString.asScannableString cs
        step t@(count, oN) (c:str)
            | c == '\'' = step t str
            | c `elem` digitsForBase base = step (count + 1, toInteger (digitToInt c) + (oN `asBase` base)) str
            | otherwise = if count == 0 then error $ "Incomplete integral literal at " ++ show cs else t
        step t [] = t


scanFloatingPoint :: CursoredString -> (CursoredString, PPLiteral)
scanFloatingPoint = undefined




--todo: put this in its own file
newtype Base = Base Int

asBase :: Integer -> Base -> Integer
asBase n (Base b) = n * toInteger b

isBase :: Base -> Char -> Bool
isBase base x = x `elem` digitsForBase base

isBase10 = isBase (Base 10)

digitsForBase :: Base -> [Char]
digitsForBase (Base b) 
    | b < 11 = ['0'..lastDigit]
    | otherwise = ['0'..'9'] ++ ['a' .. lastDigit] ++ ['A' .. toUpper lastDigit]
    where
        lastDigit = intToDigit (b-1)