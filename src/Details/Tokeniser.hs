module Details.Tokeniser where

import Token
import Details.Strings.Utils
import Data.Char(isSpace, isAlphaNum, isSymbol, isMark)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import Details.Strings.Scanner (scanString, scannableAsStringLiteral)
import Details.Numbers.Scanner (scannableAsIntegral, scanNumber)
import Data.Maybe (maybeToList)
import Control.Monad.State.Lazy
import Details.CursoredStringState

scanTokens :: State CursoredString [Token]
scanTokens = do
    cs <- get
    xs <- asScannableString
    let offsetPastSpace = offsetPastTabsSpaces xs
    if null xs then return [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, cursor = CursString.cursor cs, preceededBySpace = CursString.spaceBefore cs }]
    else do
        _ <- advanceChars offsetPastSpace
        directiveTok <- scanDirective
        case directiveTok of
            Nothing -> do 
                _ <- onNoDirective
                scanTokens
            Just tok -> do
                after <- if tokenType tok == TokInclude then afterIncludeToken else scanDirectiveBody
                _ <- toNextLine
                restToks <- scanTokens
                return (tok:after ++ restToks)

onNoDirective :: State CursoredString CursoredString
onNoDirective = do
    cs <- get
    if CursString.asScannableString cs `startsWith` '#' 
        then error $ "Unrecognized preprocessor directive at " ++ show cs
        else toNextLine

afterIncludeToken :: State CursoredString [Token]
afterIncludeToken = do
    cs <- advanceCharsTo offsetPastTabsSpaces
    xs <- asScannableString
    if xs `startsWith` '<'
        then do
            newCS <- advanceCharsTo (offsetPastChar '>')
            return [tokenFromCursStrings cs newCS TokChevronPath]
    else if xs `startsWith` '\"'
        then do 
            _ <- incrementChars
            newCS <- advanceCharsTo (offsetPastChar '\"')
            let between = CursString.between cs newCS
            return [Token { tokenType = TokLiteral, lexeme = between, literal = Just $ PPString between StrOrdinary False, cursor = CursString.cursor cs, preceededBySpace = False }]
    else error $ "Malformed #include directive at " ++ show cs

scanDirective :: State CursoredString (Maybe Token)
scanDirective = do
    cs <- get
    c <- eatChar
    case c of
        Just '#' -> do
            xs <- asScannableString
            let tok = xs `atStartOf` directiveTokens
            case tok of 
                Nothing -> return Nothing
                Just (found, tokType) -> do
                    newCS <- advanceChars (length found)
                    return $ Just (tokenFromCursStrings cs newCS tokType)
        _ -> return Nothing

scanDirectiveBody :: State CursoredString [Token]
scanDirectiveBody = do
    cs <- advanceCharsTo offsetPastTabsSpaces
    xs <- asScannableString
    if CursString.noMoreChars cs || startsWithEndl xs 
        then return []
        else directiveBody

directiveBody :: State CursoredString [Token]
directiveBody = do
    cs <- get
    xs <- asScannableString
    case xs `atStartOf` bodyTokens of
        Just (found, tokType) -> do
            let newTokLen = length found
            newCS <- advanceChars newTokLen
            let newTok = tokenFromCursStrings cs newCS tokType
            rest <- scanDirectiveBody
            return (newTok : rest)
        Nothing -> do
            unrecognized <- scanUnrecognizedToken
            rest <- scanDirectiveBody
            return $ maybeToList unrecognized ++ rest


scanUnrecognizedToken :: State CursoredString (Maybe Token)
scanUnrecognizedToken = do
    cs <- get
    str <- asScannableString
    if CursString.noMoreChars cs then error $ "Empty string passed to scanUnrecognizedToken at" ++ show cs ++ "! This should never happen!"
    else if scannableAsIntegral str then do
        lit <- scanNumber
        return $ Just (Token TokLiteral "" (Just lit) (CursString.cursor cs) (CursString.spaceBefore cs))
    else if isSpace (head str) then do
        _ <- incrementChars
        return Nothing
    else if scannableAsStringLiteral str then do
        lit <- scanString
        newCS <- get
        return $ Just (tokenFromCursStrings cs newCS TokLiteral `withLiteral` lit)
    else do
        Just <$> scanName --anything else is a TokSymbolName  

scanName :: State CursoredString Token
scanName = do
    cs <- get
    newCS <- advanceCharsTo pastSymbol
    if CursString.charDifference cs newCS /= 0
        then return $ tokenFromCursStrings cs newCS TokName
        else error $ "scanName was called on an invalid input at " ++ show cs ++ "!"

pastSymbol :: [Char] -> Int
pastSymbol [] = 0
pastSymbol str@(char:chars) = case atStartOf str bodyTokens of
    Nothing -> if isAlphaNum char || isMark char || isSymbol char || char == '_' then 1 + pastSymbol chars else 0
    Just _ -> 0

tokenFromCursStrings :: CursoredString -> CursoredString -> TokenType -> Token
tokenFromCursStrings start end tokType = Token { tokenType = tokType, lexeme = CursString.between start end, literal = Nothing, cursor = CursString.cursor start, preceededBySpace = CursString.spaceBefore start }