module Details.Strings.Scanner where

import Data.List(isPrefixOf, find)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import PPLiteral
import Details.Strings.Utils(beforeChar, offsetAtStr)
import Details.ListUtils(beforeOffset)
import Data.Char(isDigit, chr, digitToInt)
import Control.Monad.State.Lazy
import Details.CursoredStringState

import Text.Regex.TDFA

scannableAsStringLiteral :: String -> Bool
scannableAsStringLiteral xs = xs =~ "^(u8|u|U|L)?R?\"" --maybe any one of the prefixes, followed by zero or one R, followed by '\"'

scanString :: State CursoredString PPLiteral
scanString = do
    xs <- asScannableString
    let prefix = find ((`isPrefixOf` xs) . fst) prefixes
    case prefix of
        Just (str, strType) -> do
            _ <- advanceChars (length str)
            next <- scanString
            return $ next `as` strType
        Nothing -> if "\"" `isPrefixOf` xs 
            then do 
                _ <- incrementChars
                scanStringContents
            else do
                _ <- advanceChars 2 -- past "R\""
                scanRawStringContents
    where
        as l t = l `withStringType` t
        prefixes = [("u8", StrUTF8), ("u", StrUTF16), ("U", StrUTF32), ("L", StrLong)]

scanStringContents :: State CursoredString PPLiteral
scanStringContents = do
    contents <- scanStringContents'
    return PPString {ppstrContents=contents, ppstrType=StrOrdinary, ppstrRaw=False}

scanStringContents' :: State CursoredString String
scanStringContents' = do
    cs <- get
    xs <- asScannableString
    let startsWith = (==) $ head xs
    if CursString.noMoreChars cs || startsWith '\n' then errorUnfinishedString cs
    else if startsWith '\\' then do
        _ <- incrementChars
        escaped <- scanEscaped
        case escaped of
            Left err -> error ("Invalid or unrecognized escape character(s) \'" ++ err ++ "\' at " ++ show cs)
            Right scanned -> do 
                nextScanned <- scanStringContents'
                return (scanned : nextScanned)
    else if startsWith '\"' then do 
        _ <- incrementChars
        return []
    else do
        _ <- incrementChars
        nextScanned <- scanStringContents'
        return $ head xs : nextScanned

--todo: rework this, it doesn't show intent at all
scanRawStringContents :: State CursoredString PPLiteral
scanRawStringContents = do
    cs <- get
    xs <- asScannableString
    let xsBeforeEndl = takeWhile (/= '\n') xs --normally, the line break is part of the raw string literals, hence "raw". The preprocessor doesn't know about this though, for it it's just another line break
        (contentsStartOffset, beforeStart) = beforeChar '(' (take 16 xsBeforeEndl) --dcharSequence can only be 16 characters long: https://en.cppreference.com/w/cpp/language/string_literal
        dcharSequence = init beforeStart --get rid of the '('
        contentsStr = drop contentsStartOffset xsBeforeEndl
        endSuffix = ')':dcharSequence ++ "\""
        (contentsEndOffset, contents) = beforeOffset (offsetAtStr endSuffix) contentsStr
        offsetTotal = contentsStartOffset + contentsEndOffset + length endSuffix
    if null contents then error $ "Invalid raw string literal at " ++ show cs
    else do
        _ <- advanceChars offsetTotal
        return PPString {ppstrContents=contents, ppstrType=StrOrdinary, ppstrRaw=True}
        

scanEscaped :: State CursoredString (Either String Char)
scanEscaped = do
    cs <- get
    xs <- asScannableString
    let startsWith c = first == c
        first = head xs
    if CursString.noMoreChars cs then return $ Left ['\0']
    else if startsWith 'u' then error "u not implemented"
    else if startsWith 'U' then error "U not implemented"
    else if startsWith 'X' then error "X not implemented"
    else if isDigit first then do
        Right <$> onEscapedDigit
    else case lookup first escapes of
        Nothing -> return $ Left [first]
        Just c -> do 
            _ <- incrementChars
            return $ Right c
    where
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

onEscapedDigit :: State CursoredString Char
onEscapedDigit = do
    xs <- asScannableString
    let trimmed = take 3 xs --octal escapes are only 3 digits long, so "\1013" is "A3"
        scannable = takeWhile (`elem` ['0'..'7']) trimmed
        offsetPast = length scannable
        scanned = foldr (\c n -> digitToInt c + n*8) 0 scannable
    _ <- advanceChars offsetPast
    return $ chr scanned

errorUnfinishedString :: Show a1 => a1 -> a2
errorUnfinishedString cs = error $ "unfinished string literal at " ++ show cs