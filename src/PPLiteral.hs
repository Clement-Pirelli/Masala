module PPLiteral where

data PPLiteral = PPString String
    | PPFloat Double
    | PPDouble Double
    | PPInt Int
    | PPChar Char
    deriving (Show, Eq)