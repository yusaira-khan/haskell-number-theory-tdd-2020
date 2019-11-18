module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

checkRepr ::(Int,[Int]) -> Expectation
checkRepr (num,repr) = shouldBe (SB.getSparseBinary num) repr
emptySparse :: SpecWith ()
emptySparse = it "Empty" $ checkRepr (0,[])
testOne :: SpecWith ()
testOne = it "One" $ checkRepr (1,[1])
testNegOne :: SpecWith ()
testNegOne = it "Negative One needs multi component newtype" $ checkRepr (-1,undefined)
testTwo :: SpecWith ()
testTwo = it "Two" $ checkRepr (2,[2])
testThree :: SpecWith ()
testThree = it "Three" $ checkRepr (3,[1,2])
sbTest :: IO()
sbTest = do
   hspec $ do
     describe "Sparse" $ do
       emptySparse
       testNegOne
       testOne
       testTwo
       testThree
exampleTest :: IO()
exampleTest = do
   hspec $ do
     describe "Addition" $ do
       it "1 + 1 is greater than 1" $ do
         (1 + 1) > 1 `shouldBe` True
       it "2 + 2 is equal to 4" $ do
         2 + 2 `shouldBe` 4
       it "x + 1 is always greater than x" $ do
         property $ \x -> x + 1 > (x :: Int)
   L.someFunc

main :: IO ()
main = do
  sbTest
