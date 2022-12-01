module Tokeniser where

import qualified Details.Tokeniser as Details(scanTokens)
import CursoredString ( newCursoredString )
import Token ( Token )
import Control.Monad.State.Lazy

scanTokens :: String -> [Token]
scanTokens xs = evalState (Details.scanTokens False) (newCursoredString xs) 