module Inputs.DummyNodes where

import Test.Hspec
import Details.Parser
import Parser
import Details.TokenParser
import Token
import Tokeniser
import Node
import Data.Either
import Data.Maybe(listToMaybe)
import TextCursor
import Control.Monad ( zipWithM_ )
import qualified Details.MaybeUtils as MU
import qualified Details.EitherUtils as EU

dummyInclude :: Node
dummyInclude = dummyNode { contents = Include { path = "", form = QuotedInclude } }

dummyDefine :: Maybe [Node] -> Node -> Node
dummyDefine params cnts = dummyNode{ contents=Define{ symbol = dummySymbol, params = params, defineContents = Right cnts}}

dummyEmptyDefine :: Maybe [Node] -> Node
dummyEmptyDefine params = dummyNode{ contents=Define{ symbol = dummySymbol, params = params, defineContents = Left []}}

addition :: Node -> Node -> Node
addition = binOpOfType BPlus

negation :: Node -> Node -> Node
negation = binOpOfType BMinus

multiplication :: Node -> Node -> Node
multiplication = binOpOfType Multiply

division :: Node -> Node -> Node
division = binOpOfType Divide

lessThan :: Node -> Node -> Node
lessThan = binOpOfType LessThan

dummyIf :: Node -> [Node] -> [Node] -> Node
dummyIf expr b elseC = dummyNode 
    { 
        contents = If 
        { 
            expression = expr, 
            body = b, 
            elseClauses = elseC  
        } 
    }

dummyIfdef :: [Node] -> [Node] -> Node
dummyIfdef = dummyIf dummySymbol

dummyElse :: [Node] -> Node
dummyElse ns = dummyNode { contents = Else { body = ns }}

binOpOfType :: BinaryOpType -> Node -> Node -> Node
binOpOfType t l r = dummyNode { contents = BinaryOp { left = l, right = r, binaryOpType = t } }

unOpOfType :: UnaryOpType -> Node
unOpOfType t = dummyNode { contents = UnaryOp { operand = dummySymbol, unaryOpType = t } }

dummyInt :: Node
dummyInt = Node (Token TokLiteral "0" (Just $ PPInt 0) (TextCursor 0 0) False) Literal

dummyDefined :: Node
dummyDefined = dummyFunc [dummySymbol]

dummyFunc :: [Node] -> Node
dummyFunc ns = dummyNode { contents = FuncLikeApplication { operands = ns } }

dummySymbol :: Node
dummySymbol = dummyNode { contents = Symbol }

dummyNode :: Node
dummyNode = Node dummyToken Symbol
    where 
        dummyToken = Token TokEOF "" Nothing dummyCursor False 
        dummyCursor = TextCursor 0 0 