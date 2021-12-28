module CursorPosition where


data CursorPosition = CursorPosition { 
    line :: Int, 
    character :: Int } deriving(Eq)

instance Show CursorPosition where
    show curs = "CursorPosition { cursorLine = " ++ show (line curs + 1) ++ ", cursorCharacter = " ++ show (character curs) ++ " }"

addCursors :: CursorPosition -> CursorPosition -> CursorPosition
addCursors x y = CursorPosition (line x + line y) (character x + character y)

addChar :: CursorPosition -> Int -> CursorPosition
addChar (CursorPosition l c) v =  CursorPosition l (c+v)

addLine :: CursorPosition -> Int -> CursorPosition
addLine (CursorPosition l c) v =  CursorPosition (l+v) c

incrementChar :: CursorPosition -> CursorPosition
incrementChar curs = addChar curs 1

incrementLine :: CursorPosition -> CursorPosition
incrementLine curs = addLine curs 1

zero :: CursorPosition
zero = CursorPosition 0 0