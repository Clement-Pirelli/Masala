module Details.Tokeniser where

import Token
import StrUtils
import Data.Char(isSpace)
import CursorPosition(CursorPosition)
import CursoredString(CursoredString)
import qualified CursorPosition as CursPos
import qualified CursoredString as CursString
import Data.Bifunctor(second)

scanTokens :: CursoredString -> Bool -> [Token]
scanTokens (CursString.CursoredString [] curs) spaceBefore = [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, cursor = curs, preceededBySpace = spaceBefore }]
scanTokens cursStr spaceBefore = case directiveTok of
    Nothing -> onNoDirective directiveCursStr
    Just (bodyStr, tok) -> tok : onDirectiveToken bodyStr 
    where
        directiveTok = scanDirective directiveCursStr beforeDirectiveIsWhiteSpace
        directiveCursStr = CursString.advanceChars cursStr offsetPastSpace
        beforeDirectiveIsWhiteSpace = offsetPastSpace > 0
        offsetPastSpace = offsetPastTabsSpaces (CursString.contents cursStr)

onNoDirective :: CursoredString  -> [Token]
onNoDirective cs@(CursString.CursoredString str curs)
    | head str == '#' = error $ "Unrecognized preprocessor directive at " ++ show curs
    | otherwise = scanTokens newStr True
    where
        newStr = CursString.incrementLine (CursString.advanceCharsTo offsetPastEndl cs)

onDirectiveToken :: CursoredString  -> [Token]
onDirectiveToken cs = bodyToks ++ restToks
    where
        restToks = scanTokens restStr False
        (restStr, bodyToks) = scanDirectiveBody cs False

scanDirective :: CursoredString -> Bool -> Maybe (CursoredString, Token)
scanDirective cs@(CursString.CursoredString xs curs) spaceBefore = do
    tok <- xs `atStartOf` directiveTokens
    let lexm = fst tok
    let len = length lexm
    let newStr = CursString.advanceChars cs len
    return (newStr, Token { tokenType = snd tok, lexeme = lexm, literal = Nothing, cursor = curs, preceededBySpace = spaceBefore })

scanDirectiveBody :: CursoredString  -> Bool -> (CursoredString, [Token])
scanDirectiveBody cs@(CursString.CursoredString [] curs) _ = (cs, [])

scanDirectiveBody cs@(CursString.CursoredString ('\\':_) _) spaceBefore = 
    handleMultiLine cs spaceBefore

scanDirectiveBody cs@(CursString.CursoredString xs curs) spaceBefore
    | startsWithEndl xs = ((CursString.incrementLine . CursString.advanceChars cs) (offsetPastEndl xs), [])
    | otherwise = case xs `atStartOf` bodyTokens of
        Just (lexm, tokType) -> second (newTok :) rest
            where
                rest = scanDirectiveBody restStr False
                restStr = CursString.advanceChars cs newTokLen
                newTok = Token { tokenType = tokType, lexeme = lexm, literal = Nothing, cursor = curs, preceededBySpace = spaceBefore }
                newTokLen = length lexm
        Nothing -> scanDirectiveBody (CursString.incrementChars cs) (isSpace $ head xs)

handleMultiLine :: CursoredString -> Bool -> (CursoredString , [Token])
handleMultiLine cs@(CursString.CursoredString str@('\\':xs) curs) spaceBefore = if isAtEndl
    then scanDirectiveBody pastEndStr spaceBefore
    else scanDirectiveBody (CursString.incrementChars cs) False
    where
        pastEndStr = CursString.incrementLine $ CursString.advanceCharsTo offsetPastEndl cs
        pastWhiteSpaceStr = CursString.advanceChars cs offsetPastWhiteSpace
        isAtEndl = startsWithEndl xsPastWhiteSpace
        (offsetPastWhiteSpace, xsPastWhiteSpace) = pastTabsSpaces xs
handleMultiLine c _ = error ("handleMultiLine called on non-multiline character at: " ++ show c ++ "! This should never happen.")