module SpecUtils where

import Test.Hspec
import Data.Foldable(traverse_)
import Data.Tuple(uncurry)

testAll :: (a -> SpecWith c) -> [a] -> SpecWith c
testAll = traverse_

testAll2 :: (a -> b -> SpecWith c) -> [(a, b)] -> SpecWith c
testAll2 = traverse_ . uncurry

testAll3 :: (a -> b -> c -> SpecWith d) -> [(a, b, c)] -> SpecWith d
testAll3 = traverse_ . uncurry3
    where uncurry3 f (x, y, z) = f x y z
