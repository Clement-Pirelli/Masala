module CursoredString where

import CursorPosition as Curs

data CursoredString = CursoredString { 
    contents :: String, 
    cursor :: Curs.CursorPosition
    } deriving(Show, Eq)

advanceChars :: CursoredString -> Int -> CursoredString
advanceChars (CursoredString str curs) offset = CursoredString{ contents = drop offset str, cursor = Curs.addChar curs offset }

advanceCharsTo :: (String -> Int) -> CursoredString -> CursoredString
advanceCharsTo f (CursoredString str curs) = CursoredString (drop to str) (Curs.addChar curs to) 
    where
        to = f str

incrementLine :: CursoredString -> CursoredString
incrementLine (CursoredString str curs) = CursoredString str (Curs.incrementLine curs)

incrementChars :: CursoredString -> CursoredString
incrementChars curs = advanceChars curs 1

furtherThan :: CursoredString -> CursoredString -> Bool
furtherThan (CursoredString _ (CursorPosition _ a)) (CursoredString _ (CursorPosition _ b)) = a > b

newCursoredString str = CursoredString str Curs.zero