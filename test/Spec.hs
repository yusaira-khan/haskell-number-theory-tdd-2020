module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

checkRepr ::(Int,[Int]) -> Expectation
checkRepr (num,repr) = shouldBe (SB.toEnum' num) repr
testToEnum :: (String,Int,[Int]) -> SpecWith ()
testToEnum (name,num,enum) = it name $ checkRepr (num,enum)
sbTest :: IO()
sbTest = do
   hspec $ do
     describe "Sparse Binary To Enum Test" $ do
       testToEnum ("Negative One (needs multicomponent new type)" , -1 , undefined)
       testToEnum ("Zero" , 0 , [])
       testToEnum ("One" , 1 , [1])
       testToEnum ("Three" , 2 , [2])
       testToEnum ("Three" , 3 , [1,2])
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
