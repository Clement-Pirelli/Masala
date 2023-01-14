module Node where

import Token

data IncludeForm = QuotedInclude | ChevronInclude deriving(Show, Eq)
data BinaryOpType = Concatenate | Multiply | Divide | Modulo | BPlus | BMinus | ShiftLeft | ShiftRight | Xor | And | Or | BitAnd | BitOr | GreaterThan | LessThan | EqualTo | GreaterThanEqual | LessThanEqual | NotEqual deriving(Show, Eq)
data UnaryOpType = Stringify | Not | BitNot | UMinus | UPlus deriving(Show, Eq)

data NodeContents = 
      UnaryOp { operand :: Node, unaryOpType :: UnaryOpType } 
    | BinaryOp { left :: Node, right :: Node, binaryOpType :: BinaryOpType}
    | FuncLikeApplication { operands :: [Node] }
    | If { expression :: Node, body :: [Node], elseClause :: Maybe Node }
    | ElseIf { expression :: Node, body :: [Node] }
    | Else { body :: [Node] }
    | Include { path :: String, form :: IncludeForm }
    | Pragma [Node]
    | Define { symbol :: Node, params :: Maybe [Node], defineContents :: Either [Token] Node }
    | Undef { symbol :: Node }
    | Symbol
    | Literal
    deriving(Show)

data Node = Node { token :: Token, contents :: NodeContents } deriving(Show)


tokensToUnaryOps :: [(TokenType, UnaryOpType)]
tokensToUnaryOps = [ 
    (TokHash, Stringify),
    (TokNot, Not),
    (TokBitNot, BitNot),
    (TokMinus, UMinus),
    (TokPlus, UPlus) ]

--sorted based on precedence
tokensToBinaryOps :: [(TokenType, BinaryOpType)]
tokensToBinaryOps = [ 
    (TokHashHash, Concatenate),
    (TokStar, Multiply),
    (TokSlash, Divide),
    (TokPercent, Modulo),
    (TokPlus, BPlus),
    (TokMinus, BMinus),
    (TokShiftLeft, ShiftLeft),
    (TokShiftRight, ShiftRight),
    (TokOpeningChevron, GreaterThan),
    (TokClosingChevron, LessThan),
    (TokGreaterEqual, GreaterThanEqual),
    (TokLesserEqual, LessThanEqual),
    (TokNotEqual, NotEqual),
    (TokEquals, EqualTo),
    (TokBitAnd, BitAnd),
    (TokXor, Xor),
    (TokBitOr, BitOr),
    (TokAnd, And),
    (TokOr, Or) ]


isSymbol :: Node -> Bool
isSymbol n = case contents n of
    Symbol -> True
    _ -> False

isLiteral :: Node -> Bool
isLiteral n = case contents n of
    Literal -> True
    _ -> False

defineSymbolIs :: Node -> (Node -> Bool) -> Bool
defineSymbolIs node predicate = case contents node of
    Define symbol' _ _ -> predicate symbol'
    _ -> False

defineParamsAre :: Node -> (Maybe [Node] -> Bool) -> Bool
defineParamsAre node predicate = case contents node of 
    Define _ parameters _ -> predicate parameters
    _ -> False

defineContentsAre :: Node -> (Either [Token] Node -> Bool) -> Bool
defineContentsAre node predicate = case contents node of 
    Define _ _ nodeContents -> predicate nodeContents
    _ -> False
    
lexemeIs :: Node -> String -> Bool
(Node tok _) `lexemeIs` s = lexeme tok == s