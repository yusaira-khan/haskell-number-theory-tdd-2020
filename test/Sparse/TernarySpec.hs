module Sparse.TernarySpec (spec) where
import Test.Hspec
import qualified Sparse.General as ST
import qualified Helper as H

testList = [
      ( 1 , [1]),
      ( 2 , [2]),
      ( 3 , [3]),
      ( 4 , [1,3]),
      ( 5 , [2,3]),
      ( 6 , [6]),
      ( 7 , [1,6]),
      ( 8 , [2,6]),
      ( 9 , [9]),
      ( 10 , [1,9]),
      ( 11 , [2,9]),
      ( 12 , [3,9]),
      ( 0 , [])]
toEnumTernary :: Int -> ST.STernary
toEnumTernary a = toEnum a :: ST.STernary
makeTernaryFromList :: [Int] -> ST.STernary
makeTernaryFromList = ST.mkSTernary

toEnumFun :: (Int,[Int]) -> SpecWith ()
toEnumFun = H.testToEnum makeTernaryFromList toEnumTernary

toEnum'' :: Spec
toEnum'' = H.testGen toEnumFun "Sparse Ternary ToEnum" testList

checkStr ::(Int,String) -> SpecWith ()
checkStr  = H.checkStrReprFun (show . toEnumTernary)
stringtest :: Spec
stringtest = describe "Sparse Ternary String" $ do
  checkStr(0,"(S=[]|D=0|B=3_0)")
  checkStr(1,"(S=[1]|D=1|B=3_1)")
  checkStr(2,"(S=[2]|D=2|B=3_2)")
  checkStr(3,"(S=[3]|D=3|B=3_10)")
  checkStr(4,"(S=[1,3]|D=4|B=3_11)")
  checkStr(5,"(S=[2,3]|D=5|B=3_12)")
  checkStr(6,"(S=[6]|D=6|B=3_20)")
checkSucc :: Int->SpecWith ()
checkSucc = H.checkSucc toEnumTernary

testSucc :: Spec
testSucc = H.testGen checkSucc "Ternary Succ test" $ enumFromTo 0 50
checkInvalidCons :: ([Int],String) -> SpecWith ()
checkInvalidCons = H.checkError makeTernaryFromList
smartConsTest ::Spec
smartConsTest = describe "Smart constructor test" $ do
  checkInvalidCons ([1,1],"Incorrect order [1,1]")
  checkInvalidCons ([1,2],"Incorrect order [1,2]")
  checkInvalidCons ([3,1],"Incorrect order [3,1]")
  checkInvalidCons ([4,2,5,162,36],"Invalid elements [4,5,36]")
checkPred ::Int -> SpecWith ()
checkPred = H.checkPred toEnumTernary
predTest :: Spec
predTest = H.testGen checkPred "Ternary Pred test" $ enumFromTo 1 50

checkAdd = H.checkAdd toEnumTernary
addTest ::Spec
addTest = H.testGen checkAdd "Compare Ternary" $ H.selfzip $ enumFromTo 0 1
spec = do
  toEnum''
  stringtest
  testSucc
  smartConsTest
  predTest
  addTest
