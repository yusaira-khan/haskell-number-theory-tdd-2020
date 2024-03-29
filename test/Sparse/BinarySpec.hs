module Sparse.BinarySpec(spec) where
import Test.Hspec
import qualified Sparse.General as SB
import qualified Helper as H

enumTestList = [
 ( 1 , [1]),
 ( 2 , [2]),
 ( 3 , [1,2]),
 ( 4 , [4]),
 ( 5 , [1,4]),
 ( 6 , [2,4]),
 ( 7 , [1,2,4]),
 ( 8 , [8]),
 ( 0 , [])]

toEnumBinary :: Int -> SB.SBinary
toEnumBinary n = toEnum n :: SB.SBinary
makeBinaryFromList :: [Int]->SB.SBinary
makeBinaryFromList = SB.mkSBinary

toEnumFun :: (Int,[Int]) -> SpecWith ()
toEnumFun = H.testToEnum makeBinaryFromList toEnumBinary

toEnum'' :: Spec
toEnum'' = H.testGen toEnumFun "Sparse Binary ToEnum" enumTestList
checkStr ::(Int,String) -> SpecWith ()
checkStr  = H.checkStrReprFun (show . toEnumBinary)
stringtest :: Spec
stringtest = describe "Sparse Binary String" $ do
  checkStr (0,"(S=[]|D=0|B=2_0)")
  checkStr (1,"(S=[1]|D=1|B=2_1)")
  checkStr (2,"(S=[2]|D=2|B=2_10)")
  checkStr (3,"(S=[1,2]|D=3|B=2_11)")
  checkStr (4,"(S=[4]|D=4|B=2_100)")
checkEq ::(Int,Int,Bool) -> SpecWith ()
checkEq = H.checkEq toEnumBinary
eqtest :: Spec
eqtest = describe "Sparse Eq" $ do
  checkEq (0,0,True)
  checkEq (0,1,False)
  checkEq (1,0,False)
  checkEq (1,1,True)
  checkEq (1,2,False)
  checkEq (1,3,False)

checkSucc ::Int -> SpecWith ()
checkSucc = H.checkSucc toEnumBinary
succtest :: Spec
succtest = describe "Binary Succ" $ do
  checkSucc 0
  checkSucc 1
  checkSucc 2
  checkSucc 3
checkInvalidCons :: ([Int],String) -> SpecWith ()
checkInvalidCons = H.checkError makeBinaryFromList
smartConsTest ::Spec
smartConsTest = describe "Smart constructor test" $ do
  checkInvalidCons ([1,1],"Incorrect order [1,1]")
  checkInvalidCons ([2,1],"Incorrect order [2,1]")
  checkInvalidCons ([3,2,6],"Invalid elements [3,6]")
checkPred ::Int -> SpecWith ()
checkPred = H.checkPred toEnumBinary
predTest :: Spec
predTest = H.testGen checkPred "Binary Pred test" $ enumFromTo 1 4
checkComp :: (Int,Int) -> SpecWith ()
checkComp = H.checkComp toEnumBinary
compareTest ::Spec
compareTest = H.testGen checkComp "Compare Binary" $ H.selfzip $ enumFromTo 0 10
checkAdd = H.checkAdd toEnumBinary
addTest ::Spec
addTest = H.testGen checkAdd "Add Binary" $ H.selfzip $ enumFromTo 0 10
spec = do
  toEnum''
  stringtest
  eqtest
  succtest
  smartConsTest
  predTest
  compareTest
  addTest
