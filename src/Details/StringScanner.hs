module Details.StringScanner where

import Data.List(isPrefixOf)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Data.Bifunctor (second)
import StrUtils(offsetPastChar)
import Data.Char(isNumber)

scannableAsStringLiteral :: String -> Bool
scannableAsStringLiteral xs = any (isPrefixOf xs) ["\"", "u8\"", "R\"", "L\"", "u\"", "U\""] --todo: change this to regex since there can be any of the prefixes followed by R

scanString :: CursoredString -> (CursoredString, PPLiteral)
scanString cs
    | "\"" `isPrefixOf` xs = scanStringContents (CursString.incrementChars cs)
    | otherwise = undefined
    where
        xs = CursString.asScannableString cs

scanStringContents :: CursoredString -> (CursoredString, PPLiteral)
scanStringContents cs = (newCursStr, PPString {ppstrContents=contents, ppstrType=StrOrdinary, ppstrRaw=False})
    where
        (newCursStr, contents) = scanStringContents' cs

scanStringContents' :: CursoredString -> (CursoredString, String)
scanStringContents' cs
    | CursString.noMoreChars cs || startsWith '\n' = errorUnfinishedString cs
    | startsWith '\\' = case scanEscaped (CursString.incrementChars cs) of
            Left c -> error ("Unrecognized escape character \'" ++ [c] ++ "\' at " ++ show cs)
            Right (newCS, scanned) -> let (nextCS, nextScanned) = scanStringContents' newCS in (nextCS, scanned ++ nextScanned)
    | startsWith '\"' = (CursString.incrementChars cs, [])
    | otherwise = let (nextStr, nextScanned) = scanStringContents' (CursString.incrementChars cs) in (nextStr, head xs : nextScanned)
    where 
        startsWith c = head xs == c
        xs = CursString.asScannableString cs 

scanEscaped :: CursoredString -> Either Char (CursoredString, String)
scanEscaped cs
    | CursString.noMoreChars cs = Left '\0'
    | startsWith 'u' = undefined
    | startsWith 'U' = undefined
    | startsWith 'X' = undefined
    | isNumber first = undefined
    | otherwise = case lookup first escapes of
        Nothing -> Left first
        Just c -> Right (CursString.incrementChars cs, [c])
    where
        startsWith c = first == c
        second = head (tail xs)
        first = head xs
        xs = CursString.asScannableString cs
        escapes = [
            ('n', '\n'),
            ('?', '?'),
            ('t', '\t'),
            ('v', '\v'),
            ('\'', '\''),
            ('\\', '\\'),
            ('a', '\a'),
            ('r', '\r'),
            ('f', '\f'),
            ('b', '\b'),
            ('"', '\"') ]

errorUnfinishedString cs = error $ "unfinished string literal at " ++ show cs 