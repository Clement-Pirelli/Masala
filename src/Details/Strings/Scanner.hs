module Details.Strings.Scanner where

import Data.List(isPrefixOf, find)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Data.Bifunctor (second)
import Details.Strings.Utils(offsetPastChar, pastChar, beforeChar, beforeOffset, offsetAtStr)
import Data.Char(isDigit)

import Text.Regex.TDFA
import Debug.Trace (trace)

scannableAsStringLiteral :: String -> Bool
scannableAsStringLiteral xs = xs =~ "^(u8|u|U|L)?R?\"" --maybe any one of the prefixes, followed by zero or one R, followed by '\"'

scanString :: CursoredString -> (CursoredString, PPLiteral)
scanString cs = case prefix of
    Just p -> handlePrefix p
    Nothing -> if "\"" `isPrefixOf` xs 
        then scanStringContents (CursString.incrementChars cs)
        else scanRawStringContents (CursString.advanceChars cs 2) -- past "R\""
    where
        prefix = find ((`isPrefixOf` xs) . fst) prefixes
        handlePrefix (str, t) = scanString (CursString.advanceChars cs (length str)) `as` t
        as xs t = second (`withStringType` t) xs
        prefixes = [("u8", StrUTF8), ("u", StrUTF16), ("U", StrUTF32), ("L", StrLong)]
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

--todo: rework this, it doesn't show intent at all
scanRawStringContents :: CursoredString -> (CursoredString, PPLiteral)
scanRawStringContents cs 
    | null contents = error $ "Invalid raw string literal at " ++ show cs
    | otherwise = (CursString.advanceChars cs offsetTotal, PPString {ppstrContents= contents, ppstrType=StrOrdinary , ppstrRaw=True})
    where
        offsetTotal = contentsStartOffset + contentsEndOffset + length endSuffix
        (contentsEndOffset, contents) = beforeOffset (offsetAtStr endSuffix) contentsStr
        endSuffix = ')':dcharSequence ++ "\""
        contentsStr = drop contentsStartOffset xsBeforeEndl
        dcharSequence = init beforeStart --get rid of the '('
        (contentsStartOffset, beforeStart) = beforeChar '(' (take 16 xsBeforeEndl) --dcharSequence can only be 16 characters long: https://en.cppreference.com/w/cpp/language/string_literal
        xsBeforeEndl = takeWhile (/= '\n') (CursString.asScannableString cs) --normally, the line break is part of the raw string literals, hence "raw". The preprocessor doesn't know about this though, for it it's just another line break

scanEscaped :: CursoredString -> Either Char (CursoredString, String)
scanEscaped cs
    | CursString.noMoreChars cs = Left '\0'
    | startsWith 'u' = undefined
    | startsWith 'U' = undefined
    | startsWith 'X' = undefined
    | isDigit first = undefined
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