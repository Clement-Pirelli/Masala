module ParserSpec where

import Test.Hspec
import Parser
import Tokeniser
import Node
import Data.Either
import Inputs.Framework
import TestInputs
import Control.Monad ( zipWithM_ )
import qualified Details.MaybeUtils as MU
import qualified Details.EitherUtils as EU

spec :: Spec
spec =
    describe "Parser" $ do
        describe "parsePP" $ do
            testAll shouldError ["#define", "#if", "#ifdef 1\n#endif"]
            let shouldAllMatchMapping i = (title i, input i, expectedNodes i)
            testAll3 shouldAllMatchWith (map shouldAllMatchMapping inputs)

main :: IO ()
main = hspec spec

shouldAllMatchWith :: String -> String -> [Node] -> SpecWith ()
shouldAllMatchWith title' input expected = 
    context title' $
        it "should match expected nodes" $
            zipWithM_ treesShouldMatch (parseTokens $ scanTokens input) expected

treesShouldMatch :: Node -> Node -> Expectation
treesShouldMatch input expected =
    let mustBe = contents input
        fail name got = failure $ "expected " ++ name ++ ", got " ++ show got ++ " at node " ++ show input
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
                zipWithM_ treesShouldMatch elseC elseC'
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
                let defineMismatch = failure $ "Define parameter mismatch for node:\n" ++ show input ++ "\nGot " ++ show otherParams ++ ", expected " ++ show parameters
                MU.bothSameAnd (zipWithM_ treesShouldMatch) success defineMismatch parameters otherParams
                let contentMismatch = failure $ "Define contents mismatch for node:\n" ++ show input ++ "\nGot" ++ show otherContents ++ ", expected" ++ show defContents
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

shouldError :: String -> SpecWith ()
shouldError input = context ("with" ++ show input) $
    it "should result in an error" $
        isError `shouldBe` True
    where
        isError = not $ isRight $ (parseTokensLenient . scanTokens) input