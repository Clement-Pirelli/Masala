module Node where

import Token

data IncludeForm = Quoted | AngleBrackets
data IfType = OrdinaryIf | Ifdef | Ifndef
data BinaryOpType = Concatenate | Multiply | Divide | Xor | And | Or | BitAnd | BitOr | GreaterThan | LessThan | EqualTo | GreaterThanEqual | LessThanEqual | NotEqual 
data UnaryOpType = Stringify | Not | BitNot | Minus | Plus

data NodeContents = 
      UnaryOp { op :: Node, unaryOpType :: UnaryOpType } 
    | BinaryOp { left :: Node, right :: Node, binaryOpType :: BinaryOpType}
    | If { expression :: Node, body :: [Node], ifType :: IfType, elseClause :: Maybe Node }
    | Elif { expression :: Node, body :: [Node] }
    | Else { body :: [Node] }
    | Include { path :: Node, form :: IncludeForm }
    | Pragma [Node]
    | Define { name :: Node, params :: Maybe [Node], defineContents :: Either [Token] [Node] }
    | Undef { name :: Node }
    | Symbol
    | Literal

data Node = Node Token NodeContents