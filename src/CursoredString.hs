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

import CursorPosition as Curs
import Details.Strings.Utils(offsetPastEndl, pastTabsSpaces, startsWithEndl, startsWith)

data CursoredString = CursoredString {
    contents :: String,
    cursor :: Curs.CursorPosition
    } deriving(Show, Eq)

newCursoredString :: String -> CursoredString
newCursoredString str = CursoredString str Curs.zero

asScannableString :: CursoredString -> String
asScannableString cs = replaceCRLF $ helper (contents cs)
                      where
                          helper str | null str = []
                                     | otherwise = let (before, after) = splitOnMultiline str in before ++ helper after

noMoreChars :: CursoredString -> Bool
noMoreChars = null . asScannableString

advanceChars :: CursoredString -> Int -> CursoredString
advanceChars cs@(CursoredString str curs) offset
    | offset <= 0 = cs
    | mightStartWithMultiline =
        if startsWithEndl strPastSpaces
            then advanceBy (1 + offsetTotal) offset --keep going past multiline, ignoring it
            else onNormalCharacter
    | startsWithEndl str = CursoredString.incrementLine $ advanceBy (offsetPastEndl str) decrementedOffset
    | otherwise = onNormalCharacter
        where
            offsetTotal = offsetPastSpaces + offsetPastEndl strPastSpaces
            (offsetPastSpaces, strPastSpaces) = pastTabsSpaces (tail str)
            onNormalCharacter = advanceBy 1 decrementedOffset
            decrementedOffset = offset - 1
            advanceBy contentsToDrop newOffset = advanceChars (CursoredString { contents = drop contentsToDrop str, cursor = Curs.addChar curs contentsToDrop }) newOffset
            mightStartWithMultiline = str `startsWith` '\\'

advanceCharsTo :: (String -> Int) -> CursoredString -> CursoredString
advanceCharsTo f cs = advanceChars cs to
    where
        to = f (asScannableString cs)

incrementChars :: CursoredString -> CursoredString
incrementChars curs = advanceChars curs 1

toNextLine :: CursoredString -> CursoredString
toNextLine = advanceCharsTo offsetPastEndl

furtherThan :: CursoredString -> CursoredString -> Bool
furtherThan (CursoredString _ (CursorPosition _ a)) (CursoredString _ (CursorPosition _ b)) = a > b

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

splitOnMultiline :: String -> (String, String)
splitOnMultiline [] = ([], [])
splitOnMultiline (x:xs)
    | x == '\\' = let (_, strPastSpaces) = pastTabsSpaces xs in if startsWithEndl strPastSpaces 
        then ([], drop (offsetPastEndl strPastSpaces) strPastSpaces) 
        else passthrough
    | otherwise = passthrough
        where
            passthrough = let (before, after) = splitOnMultiline xs in (x:before, after)

incrementLine :: CursoredString -> CursoredString
incrementLine (CursoredString xs curs) = CursoredString { contents = xs, cursor = Curs.incrementLine curs }