{-# LANGUAGE InstanceSigs #-}
module Details.Parser where

import Cursored
import Token
import Data.Bifunctor


-- mostly taken from https://injuly.in/blog/monparsing/
newtype Parser a = Parser { parse :: Cursored Token -> Either String (a, Cursored Token) }

instance Monad Parser where
  parser >>= f = Parser bound
    where
        bound curs = parse parser curs >>= handleF
        handleF (xs, newCurs) = let parser' = f xs
                                    in parse parser' newCurs
  return input = Parser (\curs -> Right (input, curs))

instance Applicative Parser where
    pure = return
    p1 <*> p2 = Parser $ \inp -> do
      (f, inp') <- parse p1 inp
      (a, inp'') <- parse p2 inp'
      return (f a, inp'')

instance Functor Parser where
    fmap f p = Parser (fmap (first f) . parse p) 

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser $ \input -> parse p input <> parse q input

result :: a -> Parser a
result = return

oops :: String -> Parser a
oops err = Parser (\toks -> Left (err ++ " at token " ++ (show . peek) toks))

item :: Parser Token
item = Parser (Right . eat)

satisfy :: String -> (Token -> Bool) -> Parser Token
satisfy err predicate = do
    x <- item
    if predicate x 
        then return x 
        else oops err

exclude :: String -> (Token -> Bool) -> Parser Token
exclude err predicate = satisfy err (not . predicate)

and' :: Parser a -> Parser b -> Parser (a, b)
p `and'` q = do
    x <- peeked p
    y <- q
    return (x, y)

validateWithThen :: Parser a -> Parser b -> Parser b
validateWithThen validator parser = do
                _ <- peeked validator
                parser

many' :: Parser a -> Parser [a]
many' parser = do
    x  <- parser -- apply p once
    xs <- many' parser <|> return [] -- recursively apply parser as many times as possible
    return (x:xs) 

then' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
then' combine p q =
    p >>= \x ->
        q >>= \xs ->
            return $ combine x xs

thenList :: Parser a -> Parser [a] -> Parser [a]
thenList = then' (:)

-- Accept a list of sequences forming an `a`, separated by sequences forming a `b`.
separatedBy :: Parser a -> Parser b -> Parser [a]
parser `separatedBy` separator = do
    x <- parser
    xs <- many' (separator >> parser)
    return (x : xs)

surroundedBy :: Parser a -> Parser b -> Parser c -> Parser b
surroundedBy open parser close = do
    _ <- open
    x <- parser 
    _ <- close
    return x

optional :: Parser a -> Parser (Maybe a)
optional parser = Parser p
    where
        p toks = case parse parser toks of
            (Right (x, xs)) -> Right (Just x, xs)
            (Left _) -> Right (Nothing, toks)

peeked :: Parser a -> Parser a
peeked parser = Parser p
    where
        p toks = do
            (x, _) <- parse parser toks
            return (x, toks) 