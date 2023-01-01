module Details.Numbers.Scanner where

import CursoredString(CursoredString)
import PPLiteral
import Details.Numbers.Base
import Data.Char (toLower, digitToInt, isNumber)
import Data.List (isPrefixOf)
import Text.Regex.TDFA
import Control.Monad.State.Lazy
import Details.CursoredStringState

scannableAsFloatingPoint :: String -> Bool
scannableAsFloatingPoint xs = xs =~ decimal
    where
        decimal = "(\\d*\\.)" --Todo: can't really use regex here

scannableAsIntegral :: String -> Bool
scannableAsIntegral [] = False
scannableAsIntegral (x:_) = isNumber x

scanNumber :: State CursoredString PPLiteral
scanNumber = do --todo: fix this
    xs <- asScannableString
    let lowerxs = map toLower xs
    let startsWith' y = y `isPrefixOf` lowerxs
    if isFloatingPoint then scanFloatingPoint
    else if startsWith' "0b" then do 
        _ <- advanceChars 2
        scanBinary
    else if startsWith' "0x" then do
        _ <- advanceChars 2
        scanHexa
    else if startsWith' "0" then scanOctal -- no need to advance, and it simplifies things for error reporting on incomplete int literals
    else scanDecimal
    where
        
        isFloatingPoint = False --Figure out a way to do this, stat. Probably a regex again
        

scanBinary :: State CursoredString PPLiteral
scanBinary = scanInteger ( IntegerScan (Base 2) ['\''])
scanHexa :: State CursoredString PPLiteral
scanHexa= scanInteger ( IntegerScan (Base 16) ['\''])
scanOctal :: State CursoredString PPLiteral
scanOctal = scanInteger ( IntegerScan (Base 8) ['\''])
scanDecimal :: State CursoredString PPLiteral
scanDecimal = scanInteger ( IntegerScan (Base 10) ['\''])


data IntegerScan = IntegerScan Base [Char]

scanInteger :: IntegerScan -> State CursoredString PPLiteral
scanInteger (IntegerScan base ignore) = do
    cs <- get
    xs <- asScannableString
    let (offset, n) = step (0, 0) xs cs
    _ <- advanceChars offset
    return $ PPInt n
    where
        step t@(count, oN) (c:str) scanned
            | c `elem` ignore = step t str scanned
            | c `isBase` base = step (count + 1, toInteger (digitToInt c) + (oN `asBase` base)) str scanned
            | otherwise = if count > 0 then t else error $ "Incomplete integral literal at " ++ show scanned
        step t [] _ = t



scanFloatingPoint :: State CursoredString PPLiteral
scanFloatingPoint = undefined