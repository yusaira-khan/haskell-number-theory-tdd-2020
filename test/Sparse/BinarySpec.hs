module Sparse.BinarySpec(spec) where
import Test.Hspec
import Sparse.Binary as SB
import Helper as H
import Control.Exception(evaluate)

enumTestList = [
 ("One" , 1 , [1]),
 ("Two" , 2 , [2]),
 ("Three" , 3 , [1,2]),
 ("Four" , 4 , [4]),
 ("Five" , 5 , [1,4]),
 ("Six" , 6 , [2,4]),
 ("Seven" , 7 , [1,2,4]),
 ("Eight" , 8 , [8]),
 ("Zero" , 0 , [])]

toEnumFun :: (String,Int,[Int]) -> SpecWith ()
toEnumFun = H.testToEnum SB.toEnum'

toEnum'' :: Spec
toEnum'' = H.testGen toEnumFun "Sparse Binary ToEnum" enumTestList
checkStr ::(String,Int,String) -> SpecWith ()
checkStr  = H.checkStrReprFun (show . SB.mkSBinary . SB.toEnum')
stringtest :: Spec
stringtest = describe "Sparse Binary String" $ do
  checkStr ("Zero",0,"(S=[]|D=0|B=2_0)")
  checkStr ("One",1,"(S=[1]|D=1|B=2_1)")
  checkStr ("Two",2,"(S=[2]|D=2|B=2_10)")
  checkStr ("Three",3,"(S=[1,2]|D=3|B=2_11)")
  checkStr ("Four",4,"(S=[4]|D=4|B=2_100)")
sb n = toEnum n :: SB.SBinary
checkEq ::(String,Int,Int,Bool) -> SpecWith ()
checkEq (name,num1,num2,truth) = it name $ ((sb num1) == (sb num2)) `shouldBe` truth
eqtest :: Spec
eqtest = describe "Sparse Eq" $ do
  checkEq ("ZeroZero",0,0,True)
  checkEq ("ZeroOne",0,1,False)
  checkEq ("OneZero",1,0,False)
  checkEq ("OneOne",1,1,True)
  checkEq ("OneTwo",1,2,False)
  checkEq ("OneThree",1,3,False)

checkSucc ::(String,Int) -> SpecWith ()
checkSucc (name,num)= it name $ (succ (sb num)) `shouldBe` (sb (succ num))
succtest :: Spec
succtest = describe "Binary Succ" $ do
  checkSucc ("Zero",0)
  checkSucc ("One",1)
  checkSucc ("Two",2)
  checkSucc ("Three",3)
checkInvalidCons :: ([Int],String) -> SpecWith ()
checkInvalidCons (value, errorName) =
  it errorName $ (evaluate $ SB.mkSBinary value) `shouldThrow` errorCall errorName
checkAsBig :: (String,(Bool,Int),Int,(Bool,Int)) -> SpecWith ()
checkAsBig (s,t1,n,t2)= it s $ (SB.asBigAs t1 n) `shouldBe` t2
asBigAsTest  :: Spec
asBigAsTest  = describe "as big as" $ do
  checkAsBig ("simplest",(True, 0),1,(True,1))
  checkAsBig ("False",(False, 1),2,(False,1))
  checkAsBig ("Less2",(True, 1),2,(True,2))
  checkAsBig ("Less4",(True, 1),4,(True,4))
  checkAsBig ("Equal",(True, 1),1,(False,1))
  checkAsBig ("Greater",(True, 2),1,(False,2))
smartConsTest ::Spec
smartConsTest = describe "Smart constructor test" $ do
  checkInvalidCons ([1,1],"Incorrect order [1,1]")
  checkInvalidCons ([2,1],"Incorrect order [2,1]")
  checkInvalidCons ([3,2,6],"Invalid elements [3,6]")
checkPred ::(String,Int) -> SpecWith ()
checkPred (name,num)= it name $ (pred (sb num)) `shouldBe` (sb (pred num))
predTest :: Spec
predTest = describe "Binary Pred test"  $ do
  checkPred ("One",1)
  checkPred ("Two",2)
  checkPred ("Three",3)
  checkPred ("Four",4)
spec = do
  toEnum''
  stringtest
  eqtest
  succtest
  smartConsTest
  asBigAsTest
  predTest
