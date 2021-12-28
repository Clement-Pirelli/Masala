module Tokeniser where

import qualified Details.Tokeniser as Details(scanTokens)
import CursoredString ( newCursoredString )
import Token ( Token )

scanTokens :: String -> [Token]
scanTokens xs = Details.scanTokens (newCursoredString xs) False