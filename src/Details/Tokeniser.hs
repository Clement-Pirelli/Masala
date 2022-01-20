module Details.Tokeniser where

import Token
import Details.Strings.Utils
import Data.Char(isSpace, isNumber, isLetter, isAlphaNum, isSymbol, isMark)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import Data.Bifunctor(second)
import Details.Strings.Scanner (scanString, scannableAsStringLiteral)
import Details.Numbers.Scanner (scannableAsIntegral, scanNumber)
import Data.Maybe (listToMaybe)

tokenFromCursStrings :: CursoredString -> CursoredString -> TokenType -> Bool -> Token
tokenFromCursStrings start end tokType spaceBefore = Token { tokenType = tokType, lexeme = CursString.between start end, literal = Nothing, cursor = CursString.cursor start, preceededBySpace = spaceBefore }

scanTokens :: CursoredString -> Bool -> (CursoredString, [Token])
scanTokens cs spaceBefore
    | CursString.noMoreChars cs = (cs, [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, cursor = CursString.cursor cs, preceededBySpace = spaceBefore }])
    | otherwise = case directiveTok of
        Nothing -> onNoDirective directiveCursStr
        Just (bodyStr, tok) -> second (tok :) (afterDirectiveToken bodyStr)
        where
            directiveTok = scanDirective directiveCursStr beforeDirectiveIsWhiteSpace
            directiveCursStr = CursString.advanceChars cs offsetPastSpace
            beforeDirectiveIsWhiteSpace = offsetPastSpace > 0
            offsetPastSpace = offsetPastTabsSpaces (CursString.asScannableString cs)

onNoDirective :: CursoredString -> (CursoredString, [Token])
onNoDirective cs
    | xs `startsWith` '#' = error $ "Unrecognized preprocessor directive at " ++ show cs
    | otherwise = scanTokens newCS True
    where
        newCS = CursString.toNextLine (CursString.advanceCharsTo offsetPastEndl cs)
        xs = CursString.asScannableString cs

afterDirectiveToken :: CursoredString -> (CursoredString, [Token])
afterDirectiveToken cs = second (bodyToks ++) restToks
    where
        restToks = scanTokens restStr False
        (restStr, bodyToks) = scanDirectiveBody cs False

scanDirective :: CursoredString -> Bool -> Maybe (CursoredString, Token)
scanDirective cs spaceBefore = do
    let xs = CursString.asScannableString cs
    h <- listToMaybe xs
    if h /= '#' then Nothing else do
        (found, tokType) <- tail xs `atStartOf` directiveTokens
        let len = length found + 1
            newCS = CursString.advanceChars cs len
        return (newCS, tokenFromCursStrings cs newCS tokType spaceBefore)

scanDirectiveBody :: CursoredString -> Bool -> (CursoredString, [Token])
scanDirectiveBody cs spaceBefore
    | CursString.noMoreChars cs = (cs, [])
    | startsWithEndl xs = (CursString.toNextLine cs, [])
    | otherwise = case xs `atStartOf` bodyTokens of
        Just (found, tokType) -> second (newTok :) rest
            where
                rest = scanDirectiveBody newCS False
                newTok = tokenFromCursStrings cs newCS tokType spaceBefore
                newCS = CursString.advanceChars cs newTokLen
                newTokLen = length found
        Nothing -> case scanUnrecognizedToken cs spaceBefore of
            (newStr, space, Just unrTok) -> second (unrTok :) $ scanDirectiveBody newStr space
            (newStr, space, Nothing) -> scanDirectiveBody newStr space
    where xs = CursString.asScannableString cs

scanUnrecognizedToken :: CursoredString -> Bool -> (CursoredString, Bool, Maybe Token)
scanUnrecognizedToken cs spaceBefore
    | CursString.noMoreChars cs = error $ "Empty string passed to scanUnrecognizedToken at" ++ show cs ++ "! This should never happen!"
    | scannableAsIntegral str = let (newCS, lit) = scanNumber cs in (newCS, False, Just (Token TokLiteral "" (Just lit) (CursString.cursor cs) spaceBefore))
    | isSpace firstLetter = (CursString.incrementChars cs, True, Nothing)
    | scannableAsStringLiteral str = let (newCS, lit) = scanString cs in (newCS, False, Just (tokenFromCursStrings cs newCS TokLiteral spaceBefore `withLiteral` lit))
    | otherwise = let (newCS, tok) = scanName cs spaceBefore in (newCS, False, return tok) --anything else is a TokSymbolName  
    where
        firstLetter = head str
        str = CursString.asScannableString cs

scanName :: CursoredString -> Bool -> (CursoredString, Token)
scanName cs spaceBefore = if offset /= 0
        then (newCS, tokenFromCursStrings cs newCS TokName spaceBefore)
        else error $ "scanName was called on an invalid input at " ++ show cs ++ "!"
    where
        newCS = CursString.advanceChars cs offset
        offset = pastSymbol xs
        xs = CursString.asScannableString cs
        pastSymbol [] = 0
        pastSymbol str@(x:xs) = case atStartOf str bodyTokens of
            Nothing -> if isAlphaNum x || isMark x || isSymbol x || x == '_' then 1 + pastSymbol xs else 0
            Just _ -> 0