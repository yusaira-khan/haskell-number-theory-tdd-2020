module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import Sparse.Ternary as ST
import Helper as H
import qualified Sparse.BinarySpec
testStToEnum :: (String,Int,[Int]) -> SpecWith ()
testStToEnum = testToEnum ST.toEnum'
stTest :: Spec
stTest = do
     describe "Sparse Ternary To Enum Test" $ do
       let testList = [
             ("One" , 1 , [1]),
             ("Two" , 2 , [2]),
             ("Three" , 3 , [3]),
             ("Four" , 4 , [1,3]),
             ("Five" , 5 , [2,3]),
             ("Six" , 6 , [6]),
             ("Seven" , 7 , [1,6]),
             ("Eight" , 8 , [2,6]),
             ("Nine" , 9 , [9]),
             ("Ten" , 10 , [1,9]),
             ("Eleven" , 11 , [2,9]),
             ("Twelve" , 12 , [3,9]),
             ("Zero" , 0 , [])]
         in bindAllList testStToEnum testList
exampleTest :: Spec
exampleTest = do
--    hspec $ do
     describe "Addition" $ do
       it "1 + 1 is greater than 1" $ do
         (1 + 1) > 1 `shouldBe` True
       it "2 + 2 is equal to 4" $ do
         2 + 2 `shouldBe` 4
       it "x + 1 is always greater than x" $ do
         property $ \x -> x + 1 > (x :: Int)
   -- L.someFunc

main :: IO ()
main = 
  let testlist =[Sparse.BinarySpec.spec,stTest] in bindAllList hspec testlist
