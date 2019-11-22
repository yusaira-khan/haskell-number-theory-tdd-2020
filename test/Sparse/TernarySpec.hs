module Sparse.TernarySpec (spec) where
import Test.Hspec
import Sparse.Ternary as ST
import Helper as H

testList = [
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
toEnumFun :: (String,Int,[Int]) -> SpecWith ()
toEnumFun = H.testToEnum ST.toEnum'

toEnum'' :: Spec
toEnum'' = H.testGen toEnumFun "Sparse Ternary ToEnum" testList

spec = do
  toEnum''
