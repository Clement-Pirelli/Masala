module CursoredTokens where


import Token
data CursoredTokens = CursoredTokens [Token] Int deriving(Show)

contents (CursoredTokens c _) = c ++ cycle (tail c) --infinite EOF at the end of the list
cursor (CursoredTokens _ curs) = curs

noMore :: CursoredTokens -> Bool
noMore = null . contents

advance :: CursoredTokens -> Int -> CursoredTokens
advance (CursoredTokens toks curs) offset = CursoredTokens (drop offset toks) (curs + offset)

advanceTo :: ([Token] -> Int) -> CursoredTokens -> CursoredTokens
advanceTo f c = advance c to
    where
        to = f (contents c)

increment :: CursoredTokens -> CursoredTokens
increment curs = advance curs 1

furtherThan :: CursoredTokens -> CursoredTokens -> Bool
furtherThan (CursoredTokens _ a) (CursoredTokens _ b) = a > b

between :: CursoredTokens -> CursoredTokens -> [Token]
between a b = if deltaLength > 0 
    then take deltaLength (contents a)
    else take (-deltaLength) (contents b)
    where
        deltaLength = CursoredTokens.cursor b - CursoredTokens.cursor a

