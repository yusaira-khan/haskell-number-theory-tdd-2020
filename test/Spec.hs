module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

checkRepr ::(Int,[Int]) -> Expectation
checkRepr (num,repr) = shouldBe (SB.toEnum' num) repr
testToEnum :: (String,Int,[Int]) -> SpecWith ()
testToEnum (name,num,enum) = it name $ checkRepr (num,enum)

bindAllList :: (a -> SpecWith ()) -> [a] -> SpecWith ()
bindAllList fun (a:[]) = fun a
bindAllList fun (a:as) = fun a >>  bindAllList fun as
sbTest :: IO()
sbTest = do
   hspec $ do
     describe "Sparse Binary To Enum Test" $ do
       let testList = [
             ("Zero" , 0 , []),
             ("One" , 1 , [1]),
             ("Two" , 2 , [2]),
             ("Three" , 3 , [1,2]),
             ("Negative One (needs multicomponent new type)" , -1 , undefined)]
         in bindAllList testToEnum testList
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
