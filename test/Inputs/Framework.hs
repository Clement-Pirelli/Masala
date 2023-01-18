module Inputs.Framework where

import Test.Hspec
import Data.Foldable(traverse_)
import Data.Tuple(uncurry)
import Node
import TokenType

data TestInput = TestInput { title :: String, input :: String, expectedString :: String, expectedTypes :: [TokenType], expectedNodes :: [Node] }

testAll :: (a -> SpecWith c) -> [a] -> SpecWith c
testAll = traverse_

testAll2 :: (a -> b -> SpecWith c) -> [(a, b)] -> SpecWith c
testAll2 = traverse_ . uncurry

testAll3 :: (a -> b -> c -> SpecWith d) -> [(a, b, c)] -> SpecWith d
testAll3 = traverse_ . uncurry3
    where uncurry3 f (x, y, z) = f x y z

success :: Expectation
success = True `shouldBe` True

failure :: String -> Expectation
failure = expectationFailure