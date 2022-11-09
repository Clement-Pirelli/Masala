module Node where

import Token

data IncludeForm = QuotedInclude | ChevronInclude deriving(Show, Eq)
data IfType = OrdinaryIf | Ifdef | Ifndef deriving(Show, Eq)
data BinaryOpType = Concatenate | Multiply | Divide | Xor | And | Or | BitAnd | BitOr | GreaterThan | LessThan | EqualTo | GreaterThanEqual | LessThanEqual | NotEqual deriving(Show, Eq)
data UnaryOpType = Stringify | Not | BitNot | Minus | Plus deriving(Show, Eq)

data NodeContents = 
      UnaryOp { op :: Node, unaryOpType :: UnaryOpType } 
    | BinaryOp { left :: Node, right :: Node, binaryOpType :: BinaryOpType}
    | If { expression :: Node, body :: [Node], ifType :: IfType, elseClause :: Maybe Node }
    | Elif { expression :: Node, body :: [Node] }
    | Else { body :: [Node] }
    | Include { path :: String, form :: IncludeForm }
    | Pragma [Node]
    | Define { symbol :: Node, params :: Maybe [Node], defineContents :: Either [Token] [Node] }
    | Undef { symbol :: Node }
    | Symbol
    | Literal
    deriving(Show)

data Node = Node { token :: Token, contents :: NodeContents } deriving(Show)

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

defineContentsAre :: Node -> (Either [Token] [Node] -> Bool) -> Bool
defineContentsAre node predicate = case contents node of 
    Define _ _ nodeContents -> predicate nodeContents
    _ -> False
    
lexemeIs :: Node -> String -> Bool
(Node tok _) `lexemeIs` s = lexeme tok == s