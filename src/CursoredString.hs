module CursoredString(
    CursoredString,
    newCursoredString,
    noMoreChars,
    asScannableString,
    advanceChars,
    advanceCharsTo,
    incrementChars,
    toNextLine,
    furtherThan,
    between,

    cursor
    ) where

import TextCursor as Curs
import Details.Strings.Utils(offsetPastEndl, pastTabsSpaces, startsWithEndl, startsWith, offsetPastString, pastString)
import Data.List(isPrefixOf)

data CursoredString = CursoredString {
    contents :: String,
    cursor :: Curs.TextCursor
    } deriving(Show, Eq)

newCursoredString :: String -> CursoredString
newCursoredString str = CursoredString str Curs.zero

asScannableString :: CursoredString -> String
asScannableString cs = replaceCRLF $ removeAllMultiComments $ removeAllSingleComments $ removeAllEscapedLines (contents cs)
                      where
                        removeAllEscapedLines [] = []
                        removeAllEscapedLines str = let (before, after) = splitOnEscapedLine str in before ++ removeAllEscapedLines after
                        removeAllSingleComments [] = []
                        removeAllSingleComments str@(x:xs) 
                            | startsWithSingleComment str = removeAllSingleComments $ drop (offsetPastEndl str) str
                            | otherwise = x: removeAllSingleComments xs
                        removeAllMultiComments [] = []
                        removeAllMultiComments str@(x:xs)
                            | startsWithMultiComment str = removeAllMultiComments $ drop (offsetPastString "*/" str) str
                            | otherwise = x : removeAllMultiComments xs

noMoreChars :: CursoredString -> Bool
noMoreChars = null . asScannableString

advanceChars :: CursoredString -> Int -> CursoredString
advanceChars cs@(CursoredString str curs) currOffset
    | currOffset <= 0 = cs
    | startsWithMultiComment str = 
        handleMultiComment cs currOffset
    | startsWithSingleComment str = 
        CursoredString.incrementLine $ advanceBy cs (offsetPastEndl str) currOffset
    | mightStartWithEscapedLine =
        handleEscapedLine cs currOffset
    | startsWithEndl str = CursoredString.incrementLine $ advanceBy cs (offsetPastEndl str) (currOffset - 1)
    | otherwise = advanceByOne cs currOffset
    where
        mightStartWithEscapedLine = str `startsWith` '\\'

--doesn't support \ in multiline comment, will give wrong line number if in the body or break if in \* or *\, e.g. *\n\
handleMultiComment cs@(CursoredString str _) currOffset = 
    CursoredString.addLine lineCount $ advanceBy cs charOffset currOffset
    where
        lineCount = (length . lines) beforeEnd
        (charOffset, beforeEnd) = pastString "*/" str

handleEscapedLine cs@(CursoredString str curs) currOffset 
    | startsWithEndl strPastSpaces = advanceBy cs (1 + offsetTotal) currOffset --keep going past escaped line, ignoring it
    | otherwise = advanceByOne cs currOffset
    where
        offsetTotal = offsetPastSpaces + offsetPastEndl strPastSpaces
        (offsetPastSpaces, strPastSpaces) = pastTabsSpaces (tail str)

advanceCharsTo :: (String -> Int) -> CursoredString -> CursoredString
advanceCharsTo f cs = advanceChars cs to
    where
        to = f (asScannableString cs)

incrementChars :: CursoredString -> CursoredString
incrementChars curs = advanceChars curs 1

toNextLine :: CursoredString -> CursoredString
toNextLine = advanceCharsTo offsetPastEndl

furtherThan :: CursoredString -> CursoredString -> Bool
furtherThan (CursoredString _ (TextCursor _ a)) (CursoredString _ (TextCursor _ b)) = a > b

between :: CursoredString -> CursoredString -> String
between a b = if deltaLength > 0 
    then take deltaLength (asScannableString a)
    else take (-deltaLength) (asScannableString b)
    where
        deltaLength = offsetOf b - offsetOf a 
        offsetOf = Curs.character . cursor

--internals, perhaps add to its own Details file

replaceCRLF :: String -> String
replaceCRLF ('\r':'\n':xs) = '\n': replaceCRLF xs
replaceCRLF (x:xs) = x: replaceCRLF xs
replaceCRLF [] = []

splitOnEscapedLine :: String -> (String, String)
splitOnEscapedLine [] = ([], [])
splitOnEscapedLine (x:xs)
    | x == '\\' = 
        let (_, strPastSpaces) = pastTabsSpaces xs in 
        if startsWithEndl strPastSpaces 
            then ([], drop (offsetPastEndl strPastSpaces) strPastSpaces) 
            else passthrough
    | otherwise = passthrough
        where
            passthrough = let (before, after) = splitOnEscapedLine xs in (x:before, after)

incrementLine :: CursoredString -> CursoredString
incrementLine (CursoredString xs curs) = CursoredString { contents = xs, cursor = Curs.incrementLine curs }

addLine :: Int -> CursoredString -> CursoredString
addLine i (CursoredString xs curs) = CursoredString { contents = xs, cursor = Curs.addLine curs i }


advanceByOne cs oldOffset = advanceBy cs 1 (oldOffset - 1) 
advanceBy (CursoredString s c) contentsToDrop = 
    advanceChars (CursoredString { contents = drop contentsToDrop s, cursor = Curs.addChar c contentsToDrop })

startsWithMultiComment = ("/*" `isPrefixOf`)
startsWithSingleComment = ("//" `isPrefixOf`)