module Details.CursoredStringState where

import qualified CursoredString as CursString
import CursoredString(CursoredString)
import Control.Monad.State.Lazy

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

eatChar :: State CursoredString (Maybe Char)
eatChar = do
    xs <- asScannableString
    _ <- incrementChars
    return (case xs of
        (x:_) -> Just x
        [] -> Nothing)

dup :: a -> (a, a)
dup x = (x,x)