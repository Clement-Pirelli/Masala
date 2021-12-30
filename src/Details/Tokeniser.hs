module Details.Tokeniser where

import Token
import StrUtils
import Data.Char(isSpace, isNumber, isLetter)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import Data.Bifunctor(second)

scanTokens :: CursoredString -> Bool -> [Token]
scanTokens cursStr spaceBefore
    | CursString.noMoreChars cursStr = [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, cursor = CursString.cursor cursStr, preceededBySpace = spaceBefore }]
    | otherwise = case directiveTok of
        Nothing -> onNoDirective directiveCursStr
        Just (bodyStr, tok) -> tok : afterDirectiveToken bodyStr 
        where
            directiveTok = scanDirective directiveCursStr beforeDirectiveIsWhiteSpace
            directiveCursStr = CursString.advanceChars cursStr offsetPastSpace
            beforeDirectiveIsWhiteSpace = offsetPastSpace > 0
            offsetPastSpace = offsetPastTabsSpaces (CursString.asScannableString cursStr)

onNoDirective :: CursoredString  -> [Token]
onNoDirective cs
    | head scanStr == '#' = error $ "Unrecognized preprocessor directive at " ++ show cs
    | otherwise = scanTokens newCS True
    where
        newCS = CursString.toNextLine (CursString.advanceCharsTo offsetPastEndl cs)
        scanStr = CursString.asScannableString cs 

afterDirectiveToken :: CursoredString  -> [Token]
afterDirectiveToken cs = bodyToks ++ restToks
    where
        restToks = scanTokens restStr False
        (restStr, bodyToks) = scanDirectiveBody cs False

scanDirective :: CursoredString -> Bool -> Maybe (CursoredString, Token)
scanDirective cs spaceBefore = do
    let xs = CursString.asScannableString cs
    tok <- xs `atStartOf` directiveTokens
    let lexm = fst tok
    let len = length lexm
    let newStr = CursString.advanceChars cs len
    return (newStr, Token { tokenType = snd tok, lexeme = lexm, literal = Nothing, cursor = CursString.cursor cs, preceededBySpace = spaceBefore })

scanDirectiveBody :: CursoredString  -> Bool -> (CursoredString, [Token])
scanDirectiveBody cs spaceBefore
    | CursString.noMoreChars cs = (cs, [])
    | startsWithEndl xs = (CursString.toNextLine cs, [])
    | otherwise = case xs `atStartOf` bodyTokens of
        Just (lexm, tokType) -> second (newTok :) rest
            where
                rest = scanDirectiveBody restStr False
                restStr = CursString.advanceChars cs newTokLen
                newTok = Token { tokenType = tokType, lexeme = lexm, literal = Nothing, cursor = CursString.cursor cs, preceededBySpace = spaceBefore }
                newTokLen = length lexm
        Nothing -> case scanUnrecognizedToken cs spaceBefore of
            (newStr, space, Just unrTok) -> second (unrTok :) $ scanDirectiveBody newStr space
            (newStr, space, Nothing) -> scanDirectiveBody newStr space
    where xs = CursString.asScannableString cs

scanUnrecognizedToken :: CursoredString -> Bool ->  (CursoredString, Bool, Maybe Token)
scanUnrecognizedToken cs spaceBefore
    | CursString.noMoreChars cs = error $ "Empty string passed to scanUnrecognizedToken at" ++ show cs ++ "! This should never happen!"
    | isNumber firstLetter = (undefined, False, return undefined) 
    | isSpace firstLetter = (CursString.incrementChars cs, True, Nothing)
    | otherwise = (undefined, False, return undefined) --anything else is a TokSymbol  
    where
        firstLetter = head str
        str = CursString.asScannableString cs

