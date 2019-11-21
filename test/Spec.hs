module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

checkRepr ::(Int,[Int]) -> Expectation
checkRepr (num,repr) = shouldBe (SB.toEnum' num) repr
testSbToEnum :: (String,Int,[Int]) -> SpecWith ()
testSbToEnum (name,num,enum) = it name $ checkRepr (num,enum)

bindAllList :: (a -> SpecWith ()) -> [a] -> SpecWith ()
bindAllList fun (a:[]) = fun a
bindAllList fun (a:as) = fun a >>  bindAllList fun as
sbTest :: IO()
sbTest = do
   hspec $ do
     describe "Sparse Binary To Enum Test" $ do
       let testList = [
             ("One" , 1 , [1]),
             ("Two" , 2 , [2]),
             ("Three" , 3 , [1,2]),
             ("Four" , 4 , [4]),
             ("Five" , 5 , [1,4]),
             ("Six" , 6 , [2,4]),
             ("Seven" , 7 , [1,2,4]),
             ("Eight" , 8 , [8]),
             ("Zero" , 0 , [])]
         in bindAllList testSbToEnum testList
stTest :: IO()
stTest = do
   hspec $ do
     describe "Sparse Ternary To Enum Test" $ do undefined 
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
  stTest
