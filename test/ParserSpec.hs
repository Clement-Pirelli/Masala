module ParserSpec where

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
import SpecUtils
import Control.Monad ( zipWithM_ )
import qualified Details.MaybeUtils as MU
import qualified Details.EitherUtils as EU


spec :: Spec
spec =
    describe "Parser" $ do
        describe "parsePP" $ do
            firstShouldMatchWith "#define A" (\n -> defineSymbolIs n (`lexemeIs` "A")) "having a define with symbol \"A\""
            firstShouldMatchWith "#define A() " (paramsMatch null) "having a function-like define with 0 params"
            let isLexeme = flip lexemeIs
                areRightNames = and . zipWith isLexeme ["a", "b", "c", "d"]
                hasRightParams = paramsMatch areRightNames
            firstShouldMatchWith "#define A(a, b, c, d) " hasRightParams "having a function-like define with params a b c and d"
            firstShouldMatchWith "#include \"a.h\"" (include QuotedInclude "a.h") "an ordinary include whose path is \"a.h\""
            firstShouldMatchWith "#include <a.h>" (include ChevronInclude "a.h") "a chevron include whose path is \"a.h\""
            shouldError "#define"
            shouldError "#if"
            shouldError "#ifdef 1\n#endif"
            defineBodyMatchWith "A" dummySymbol
            let addition x y = dummyNode { contents = BinaryOp { left = x, right = y, binaryOpType = BPlus } }
                multiplication x y = dummyNode { contents = BinaryOp { left = x, right = y, binaryOpType = Multiply} }
            defineBodyMatchWith "B+C" (addition dummySymbol dummySymbol)
            defineBodyMatchWith "A+B*C" (multiplication (addition dummySymbol dummySymbol) dummySymbol)
            let symbolBitNot = dummyNode { contents = UnaryOp { operand = dummySymbol, unaryOpType = BitNot } }
            defineBodyMatchWith "~A" symbolBitNot
            defineBodyMatchWith "~A+A" (addition symbolBitNot dummySymbol)
            shouldAllMatchWith "#ifdef A\n#endif" [dummyNode { contents = If { expression = dummySymbol, body = [], elseClause = Nothing } }]
            shouldAllMatchWith "#if 1\n#endif" [dummyNode { contents = If { expression = dummyInt 1, body = [], elseClause = Nothing } }]

main :: IO ()
main = hspec spec

firstShouldMatchWith :: String -> (Node -> Bool) -> String -> SpecWith ()
firstShouldMatchWith input predicate description =
    context ("with\n\t" ++ show input) $
        it ("the first parsed node should match with " ++ description) $
            let candidates =  (parseTokens . scanTokens) input
                allMatch = fmap predicate candidates
            in listToMaybe allMatch `shouldBe` Just True

defineBodyMatchWith :: String -> Node -> SpecWith ()
defineBodyMatchWith input expected =
    context ("with\n\t" ++ show input) $
        it ("the define's body should match with " ++ show expected) $
            zipWithM_ treesShouldMatch (parseTokens $ scanTokens ("#define A " ++ input)) [dummyNode{ contents=Define{ symbol = dummySymbol, params = Nothing, defineContents = Right expected}}]

shouldAllMatchWith :: String -> [Node] -> SpecWith ()
shouldAllMatchWith input expected = 
    context ("with\n\t" ++ show input) $
        it ("should all match with " ++ show expected) $
            zipWithM_ treesShouldMatch (parseTokens $ scanTokens input) expected

treesShouldMatch :: Node -> Node -> Expectation
treesShouldMatch input expected =
    let mustBe = contents input
        fail name got = failure $ "expected " ++ name ++ ", got " ++ show got
    in case contents expected of
        UnaryOp o _ -> case mustBe of
            UnaryOp o' _ -> treesShouldMatch o o'
            c -> fail "UnaryOp" c 
        BinaryOp l r _ -> case mustBe of
            BinaryOp l' r' _ -> treesShouldMatch l l' >> treesShouldMatch r r'
            c -> fail "BinaryOp" c
        FuncLikeApplication os -> case mustBe of
            FuncLikeApplication os' -> zipWithM_ treesShouldMatch os os'
            c -> fail "FuncLikeApplication" c
        If expr bodyNodes elseC -> case mustBe of
            If expr' bodyNodes' elseC' -> do
                _ <- treesShouldMatch expr expr'
                zipWithM_ treesShouldMatch bodyNodes bodyNodes'
                let elseClauseMismatch = failure $ "Else clause mismatch for If. Expected " ++ show elseC' ++ ", got " ++ show elseC
                MU.bothSameAnd treesShouldMatch success elseClauseMismatch elseC elseC'
            c -> fail "If" c
        ElseIf expr bodyNodes -> case mustBe of
            ElseIf otherExpr otherNodes -> do
                treesShouldMatch expr otherExpr
                zipWithM_ treesShouldMatch bodyNodes otherNodes
            c -> fail "ElseIf" c
        Else bodyNodes -> case mustBe of
            Else otherNodes -> zipWithM_ treesShouldMatch bodyNodes otherNodes
            c -> fail "Else" c
        Include _ _ -> case mustBe of
            Include _ _ -> success
            c -> fail "Include" c
        Pragma nodes -> case mustBe of
            Pragma otherNodes -> zipWithM_ treesShouldMatch nodes otherNodes
            c -> fail "Pragma" c
        Define symb parameters defContents -> case mustBe of
            Define otherSymb otherParams otherContents -> do
                _ <- treesShouldMatch symb otherSymb
                let defineMismatch = failure $ "Define parameter mismatch. Got " ++ show otherParams ++ ", expected " ++ show parameters
                MU.bothSameAnd (zipWithM_ treesShouldMatch) success defineMismatch parameters otherParams
                let contentMismatch = failure $ "Define contents mismatch. Got" ++ show otherContents ++ ", expected" ++ show defContents
                EU.bothSameAnd treesShouldMatch (\x y -> success) contentMismatch defContents otherContents
            c -> fail "Define" c
        Undef symb -> case mustBe of
            Undef other -> treesShouldMatch symb other
            c -> fail "Undef" c
        Symbol -> case mustBe of
            Symbol -> success
            c -> fail "Symbol" c
        Literal -> case mustBe of
            Literal -> success
            c -> fail "Literal" c

paramsMatch :: ([Node] -> Bool) -> Node -> Bool
paramsMatch f node = defineParamsAre node (maybe False f)

shouldError :: String -> SpecWith ()
shouldError input = context ("with" ++ show input) $
    it "should result in an error" $
        isError `shouldBe` True
    where
        isError = not $ isRight $ (parseTokensLenient . scanTokens) input

include :: IncludeForm -> String -> Node -> Bool
include form path node = case contents node of 
                            Include path form -> True
                            _ -> False

makeBinOfType :: BinaryOpType -> NodeContents
makeBinOfType t = BinaryOp { left = dummyNode, right = dummyNode, binaryOpType = t }

dummyInt :: Integer -> Node
dummyInt i = Node (Token TokLiteral (show i) (Just $ PPInt i) (TextCursor 0 0) False) Literal

dummySymbol :: Node
dummySymbol = dummyNode { contents = Symbol }

dummyNode :: Node
dummyNode = Node dummyToken Symbol
    where 
        dummyToken = Token TokEOF "" Nothing dummyCursor False 
        dummyCursor = TextCursor 0 0 