module Details.StringScanner where

import Data.List(isPrefixOf)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Data.Bifunctor (second)
import StrUtils(offsetPastChar)

scannableAsStringLiteral :: String -> Bool
scannableAsStringLiteral xs 
    | any (isPrefixOf xs) ["u8\"", "R\"", "L\"", "u\"", "U\""] = True --todo: change this to regex since there can be any of the prefixes followed by R
    | otherwise = False

scanString :: CursoredString -> (CursoredString, PPLiteral)
--scanString cs@(CursString.CursoredString ('u':'8':xs) _) = second (`withStringType` UTF8) (scanString (CursString.advanceChars cs 2))
--scanString cs@(CursString.CursoredString ('u':xs) _) = second (`withStringType` UTF16) (scanString (CursString.incrementChars cs))
--scanString cs@(CursString.CursoredString ('U':xs) _) = second (`withStringType` UTF32) (scanString (CursString.incrementChars cs))
--scanString cs@(CursString.CursoredString ('L':xs) _) = second (`withStringType` LONG) (scanString (CursString.incrementChars cs))
--scanString cs@(CursString.CursoredString ('R':xs) _) = scanRawStringContents (CursString.advanceChars cs 2)
--scanString cs@(CursString.CursoredString ('\"':xs) _) = scanStringContents cs
scanString _ = undefined

scanRawStringContents :: CursoredString -> (CursoredString, PPLiteral) --todo: should we really support scanning raw contents?
scanRawStringContents cs = (cs, PPString {ppstrContents="", ppstrType=ORDINARY, ppstrRaw=True})

scanStringContents :: CursoredString -> (CursoredString, PPLiteral)
--scanStringContents cs@(CursString.CursoredString xs _) = (newCursStr, PPString {ppstrContents="", ppstrType=ORDINARY , ppstrRaw=False})
--    where
--        newCursStr = CursString.advanceChars cs offset
--        offset = offsetPastChar '\"' xs
scanStringContents _ = undefined