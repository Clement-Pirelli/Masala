module Details.Tokeniser where

import Token
import Details.Strings.Utils
import Data.Char(isSpace, isAlphaNum, isSymbol, isMark)
import CursoredString(CursoredString)
import qualified CursoredString as CursString
import Details.Strings.Scanner (scanString, scannableAsStringLiteral)
import Details.Numbers.Scanner (scannableAsIntegral, scanNumber)
import Data.Maybe (listToMaybe, maybeToList)
import Control.Monad.State.Lazy
import Details.CursoredStringState

--todo: spaceBefore flags are all wrong. Instead of manually handling it here, we should most likely keep a flag on the CursoredString you can query for

scanTokens :: Bool -> State CursoredString [Token]
scanTokens spaceBefore = do
    cs <- get
    xs <- asScannableString
    let beforeDirectiveIsWhiteSpace = offsetPastSpace > 0
        offsetPastSpace = offsetPastTabsSpaces xs
    if CursString.noMoreChars cs then return [Token { tokenType = TokEOF, lexeme = "", literal = Nothing, cursor = CursString.cursor cs, preceededBySpace = spaceBefore }]
    else do
        _ <- advanceChars offsetPastSpace
        directiveTok <- scanDirective beforeDirectiveIsWhiteSpace
        case directiveTok of
            Nothing -> do 
                _ <- onNoDirective
                scanTokens True
            Just tok -> do
                after <- if tokenType tok == TokInclude then afterIncludeToken else afterDirectiveToken
                restToks <- scanTokens False
                return (tok:after ++ restToks)

onNoDirective :: State CursoredString CursoredString
onNoDirective = do
    cs <- get
    if CursString.asScannableString cs `startsWith` '#' then error $ "Unrecognized preprocessor directive at " ++ show cs
    else do
        _ <- advanceCharsTo offsetPastEndl
        toNextLine

afterIncludeToken :: State CursoredString [Token]
afterIncludeToken = do
    cs <- advanceCharsTo offsetPastTabsSpaces
    xs <- asScannableString
    if xs `startsWith` '<'
        then do
            newCS <- advanceCharsTo (offsetPastChar '>')
            return [tokenFromCursStrings cs newCS TokChevronPath False]
    else if xs `startsWith` '\"'
        then do 
            newCS <- advanceCharsTo (offsetPastChar '\"')
            let between = CursString.between cs newCS
            return [Token { tokenType = TokLiteral, lexeme = between, literal = Just $ PPString between StrOrdinary False, cursor = CursString.cursor cs, preceededBySpace = False }]
    else error $ "Malformed #include directive at " ++ show cs

afterDirectiveToken :: State CursoredString [Token]
afterDirectiveToken = scanDirectiveBody False

scanDirective :: Bool -> State CursoredString (Maybe Token)
scanDirective spaceBefore = state (\cs -> 
    case scan cs of 
        Just (cs', t) -> (Just t, cs')
        Nothing -> (Nothing, cs)
    )
    where
        scan curs = do
            let xs = CursString.asScannableString curs
            h <- listToMaybe xs
            if h /= '#' then Nothing else do
                (found, tokType) <- tail xs `atStartOf` directiveTokens
                let len = length found + 1 -- +1 for the #
                    newCS = CursString.advanceChars curs len
                return (newCS, tokenFromCursStrings curs newCS tokType spaceBefore)

scanDirectiveBody :: Bool -> State CursoredString [Token]
scanDirectiveBody spaceBefore = do
    cs <- get
    xs <- asScannableString
    if CursString.noMoreChars cs 
        then return []
        else 
            if startsWithEndl xs 
                then do 
                    _ <- toNextLine
                    return []
                else 
                    directiveBody spaceBefore

directiveBody :: Bool -> State CursoredString [Token]
directiveBody spaceBefore = do
    cs <- get
    xs <- asScannableString
    case xs `atStartOf` bodyTokens of
        Just (found, tokType) -> do
            let newTokLen = length found
            newCS <- advanceChars newTokLen
            let newTok = tokenFromCursStrings cs newCS tokType spaceBefore
            rest <- scanDirectiveBody False
            return (newTok : rest)
        Nothing -> do
            unrecognized <- scanUnrecognizedToken spaceBefore
            rest <- scanDirectiveBody spaceBefore
            return $ maybeToList unrecognized ++ rest


scanUnrecognizedToken :: Bool -> State CursoredString (Maybe Token)
scanUnrecognizedToken spaceBefore = do
    cs <- get
    str <- asScannableString
    if CursString.noMoreChars cs then error $ "Empty string passed to scanUnrecognizedToken at" ++ show cs ++ "! This should never happen!"
    else if scannableAsIntegral str then do
        lit <- scanNumber
        return $ Just (Token TokLiteral "" (Just lit) (CursString.cursor cs) spaceBefore)
    else if isSpace (head str) then do
        _ <- incrementChars
        return Nothing
    else if scannableAsStringLiteral str then do
        lit <- scanString
        newCS <- get
        return $ Just (tokenFromCursStrings cs newCS TokLiteral spaceBefore `withLiteral` lit)
    else do
        tok <- scanName spaceBefore
        return $ Just tok --anything else is a TokSymbolName  

scanName :: Bool -> State CursoredString Token
scanName spaceBefore = do
    cs <- get
    newCS <- advanceCharsTo pastSymbol
    if CursString.charDifference cs newCS /= 0
        then return $ tokenFromCursStrings cs newCS TokName spaceBefore
        else error $ "scanName was called on an invalid input at " ++ show cs ++ "!"

pastSymbol :: [Char] -> Int
pastSymbol [] = 0
pastSymbol str@(char:chars) = case atStartOf str bodyTokens of
    Nothing -> if isAlphaNum char || isMark char || isSymbol char || char == '_' then 1 + pastSymbol chars else 0
    Just _ -> 0

tokenFromCursStrings :: CursoredString -> CursoredString -> TokenType -> Bool -> Token
tokenFromCursStrings start end tokType spaceBefore = Token { tokenType = tokType, lexeme = CursString.between start end, literal = Nothing, cursor = CursString.cursor start, preceededBySpace = spaceBefore }