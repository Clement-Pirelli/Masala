module Details.Tokeniser where

import Token
import Details.Strings.Utils
import Data.Char(isSpace, isNumber, isLetter, isAlphaNum, isSymbol, isMark)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import Data.Bifunctor(second)
import Details.Strings.Scanner (scanString, scannableAsStringLiteral)

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
    | scanStr `startsWith` '#' = error $ "Unrecognized preprocessor directive at " ++ show cs
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
    | scannableAsStringLiteral str = let (newCS, lit) = scanString cs in (newCS, False, Just (Token TokLiteral "" (Just lit) (CursString.cursor cs) spaceBefore))
    | otherwise = let (newCS, tok) = scanName cs spaceBefore in (newCS, False, return tok) --anything else is a TokSymbolName  
    where
        firstLetter = head str
        str = CursString.asScannableString cs

scanName :: CursoredString -> Bool -> (CursoredString, Token)
scanName cs spaceBefore = if offset /= 0
        then (newCS, Token {tokenType=TokName, lexeme=take offset xs, literal=Nothing, cursor=CursString.cursor newCS, preceededBySpace=spaceBefore})
        else error $ "scanName was called on an invalid input at " ++ show cs ++ "!"
    where
        newCS = CursString.advanceChars cs offset
        offset = pastSymbol xs
        xs = CursString.asScannableString cs
        pastSymbol [] = 0
        pastSymbol (x:xs) = if isAlphaNum x || isMark x || isSymbol x || x == '_' then 1 + pastSymbol xs else 0