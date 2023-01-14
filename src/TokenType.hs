module TokenType where

data TokenType = 
      TokIf
    | TokElif
    | TokElifdef
    | TokElifndef
    | TokIfdef
    | TokIfndef
    | TokElse
    | TokEndif
    | TokDefine
    | TokUndef
    | TokPragma
    | TokName
    | TokChevronPath --bit of a hack for #include <stuff> to work nicely
    | TokInclude
    | TokEquals
    --Unary operators
    | TokNot | TokPlus | TokMinus | TokStar | TokPercent | TokHash | TokShiftLeft | TokShiftRight
    --Binary operators
    | TokIsEqual | TokAnd | TokOr | TokNotEqual | TokGreaterEqual | TokLesserEqual 
    | TokBitNot | TokBitAnd | TokBitOr | TokXor | TokHashHash | TokSlash
    --Blocks
    | TokOpeningParens | TokClosingParens | TokOpeningChevron | TokClosingChevron | TokOpeningBrace | TokClosingBrace | TokOpeningBracket | TokClosingBracket
    | TokDot
    | TokComma
    | TokLiteral
    | TokEOF
    deriving (Show, Eq)