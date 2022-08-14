module TextCursor where


data TextCursor = TextCursor { 
    line :: Int, 
    character :: Int } deriving(Eq)

instance Show TextCursor where
    show curs = "TextCursor { cursorLine = " ++ show (line curs + 1) ++ ", cursorCharacter = " ++ show (character curs) ++ " }"

addCursors :: TextCursor -> TextCursor -> TextCursor
addCursors x y = TextCursor (line x + line y) (character x + character y)

addChar :: TextCursor -> Int -> TextCursor
addChar (TextCursor l c) v =  TextCursor l (c+v)

addLine :: TextCursor -> Int -> TextCursor
addLine (TextCursor l c) v =  TextCursor (l+v) c

incrementChar :: TextCursor -> TextCursor
incrementChar curs = addChar curs 1

incrementLine :: TextCursor -> TextCursor
incrementLine curs = addLine curs 1

zero :: TextCursor
zero = TextCursor 0 0