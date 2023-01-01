module Details.CursoredStringState where

import qualified CursoredString as CursString
import CursoredString(CursoredString)
import Control.Monad.State.Lazy
import Details.Strings.Utils

asScannableString :: State CursoredString String
asScannableString = do
    cs <- get
    let xs = CursString.asScannableString cs
    return xs

incrementChars :: State CursoredString CursoredString
incrementChars = advanceChars 1

advanceChars :: Int -> State CursoredString CursoredString
advanceChars n = state $ \s -> dup $ CursString.advanceChars s n

advanceCharsTo :: (String -> Int) -> State CursoredString CursoredString
advanceCharsTo f = state $ \s ->
    let str = CursString.asScannableString s
    in dup $ CursString.advanceChars s (f str)

toNextLine :: State CursoredString CursoredString
toNextLine = state (dup . CursString.toNextLine)

pastChar :: Char -> State CursoredString CursoredString
pastChar c = advanceCharsTo (offsetPastChar c)

pastWhitespace :: State CursoredString CursoredString
pastWhitespace = advanceCharsTo offsetPastTabsSpaces

eatChar :: State CursoredString (Maybe Char)
eatChar = do
    r <- peekChar
    _ <- incrementChars
    return r

peekChar :: State CursoredString (Maybe Char)
peekChar = do
    xs <- asScannableString
    return (case xs of
        (x:_) -> Just x
        [] -> Nothing)

dup :: a -> (a, a)
dup x = (x,x)