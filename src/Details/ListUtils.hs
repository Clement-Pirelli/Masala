module Details.ListUtils where
import Data.List (isPrefixOf)

takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList f whole@(x:xs) 
    | f whole = whole
    | otherwise = x : takeWhileList f xs
takeWhileList _ [] = []

offsetPastSublist :: (Eq a) => [a] -> [a] -> Int
offsetPastSublist [] _ = 0
offsetPastSublist l@(_:s) xs = if xs `isPrefixOf` l then length xs else 1 + offsetPastSublist s xs 

pastOffset :: ([a] -> Int) -> [a] -> (Int, [a])
pastOffset = tupWithOffsetFunc drop
beforeOffset :: ([a] -> Int) -> [a] -> (Int, [a])
beforeOffset = tupWithOffsetFunc take

tupWithOffsetFunc :: (Int -> [a] -> [a]) -> ([a] -> Int) -> [a] -> (Int, [a]) 
tupWithOffsetFunc f g xs = (i, f i xs) 
    where i = g xs