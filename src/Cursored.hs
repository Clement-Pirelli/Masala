module Cursored where

data Cursored a = Cursored [a] Int deriving(Show)

newCursored :: [a] -> Cursored a
newCursored xs = Cursored xs 0

contents :: Cursored a -> [a]
contents (Cursored c _) = c ++ cycle (tail c) --infinite repetition at the end of the list

cursor :: Cursored a -> Int
cursor (Cursored _ curs) = curs

peek :: Cursored a -> a
peek = head . contents

advance :: Cursored a -> Int -> Cursored a
advance (Cursored l@[_] curs) offset = Cursored l (curs + offset) -- we want to keep the last element, otherwise infinite cycling at the end doesn't work
advance (Cursored as curs) offset = Cursored (drop offset as) (curs + offset)

advanceTo :: ([a] -> Int) -> Cursored a -> Cursored a
advanceTo f c = advance c to
    where
        to = f (contents c)

increment :: Cursored a -> Cursored a
increment curs = advance curs 1

eat :: Cursored a -> (a, Cursored a)
eat c = (peek c, increment c)

furtherThan :: Cursored a -> Cursored a -> Bool
furtherThan (Cursored _ a) (Cursored _ b) = a > b

between :: Cursored a -> Cursored a -> [a]
between x y = if deltaLength > 0 
    then take deltaLength (contents x)
    else take (-deltaLength) (contents y)
    where
        deltaLength = Cursored.cursor y - Cursored.cursor x

