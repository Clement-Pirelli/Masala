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

    cursor
    ) where

import CursorPosition as Curs
import Data.List(isPrefixOf)
import StrUtils(offsetPastEndl)
import ListUtils(safeHead)

data CursoredString = CursoredString { 
    contents :: String, 
    cursor :: Curs.CursorPosition
    } deriving(Show, Eq)

newCursoredString :: String -> CursoredString
newCursoredString str = CursoredString str Curs.zero

asScannableString :: CursoredString -> String
asScannableString cs = helper (contents cs)
                      where
                          helper str | null str = []
                                     | otherwise = let (before, after) = splitOnMultiline str in before ++ helper after

noMoreChars :: CursoredString -> Bool
noMoreChars = null . contents

--todo: make this elegant and not self-repeating
advanceChars :: CursoredString -> Int -> CursoredString
advanceChars cs 0 = cs
advanceChars cs@(CursoredString str curs) offset
    | "\\\r\n" `isPrefixOf` str = by $ length "\\\r\n"
    | "\\\n" `isPrefixOf` str = by $ length "\\\n"
    | "\r\n" `isPrefixOf` str = CursoredString.incrementLine $ by $ length "\r\n"
    | "\n" `isPrefixOf` str = CursoredString.incrementLine $ by $ length "\n"
    | otherwise = by 1
        where 
            by i = advanceChars (CursoredString { contents = drop i str, cursor = Curs.addChar curs i }) (offset - i)


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
        




--internals

splitOnMultiline :: String -> (String, String)
splitOnMultiline [] = ([], [])
splitOnMultiline ('\\':'\n':xs) = ([], xs)
splitOnMultiline ('\\':'\r':'\n':xs) = ([], xs)
splitOnMultiline (x:xs) = let (before, after) = splitOnMultiline xs in (x:before, after)

incrementLine :: CursoredString -> CursoredString
incrementLine (CursoredString xs curs) = CursoredString { contents = xs, cursor = Curs.incrementLine curs }