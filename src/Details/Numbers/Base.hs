module Details.Numbers.Base where

import Data.Char (intToDigit, toUpper)

newtype Base = Base Int

asBase :: Integer -> Base -> Integer
asBase n (Base b) = n * toInteger b

isBase :: Char -> Base -> Bool
isBase c base = c `elem` digitsForBase base

isBase10 :: Char -> Bool
isBase10 x = isBase x (Base 10)

digitsForBase :: Base -> [Char]
digitsForBase (Base b) 
    | b < 11 = ['0'..lastDigit]
    | otherwise = ['0'..'9'] ++ ['a' .. lastDigit] ++ ['A' .. toUpper lastDigit]
    where
        lastDigit = intToDigit (b-1)