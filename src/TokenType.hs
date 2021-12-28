module TokenType where

data TokenType = 
      TokIf
    | TokElif
    | TokIfdef
    | TokElse
    | TokEndif
    | TokDefine
    | TokPragma
    | TokSymbolName
    | TokInclude
    --Unary operators
    | TokNot | TokPlus | TokMinus | TokStar | TokHash
    --Binary operators
    | TokEqual | TokAnd | TokOr | TokNotEqual | TokGreaterEqual | TokLesserEqual 
    | TokBitAnd | TokBitOr | TokXor | TokHashHash | TokSlash

    | TokOpeningParens | TokClosingParens | TokOpeningChevron | TokClosingChevron | TokOpeningBrace | TokClosingBrace | TokOpeningBracket | TokClosingBracket
    | TokDot
    | TokComma
    | TokEOF
    | TokLiteral
    deriving (Show, Eq)