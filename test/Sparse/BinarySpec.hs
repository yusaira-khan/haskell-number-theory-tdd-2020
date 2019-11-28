module Sparse.BinarySpec(spec) where
import Test.Hspec
import Sparse.Binary as SB
import Helper as H

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
checkStr  = H.checkStrReprFun (show . SB.SBinary . SB.toEnum')
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

spec = do
  toEnum''
  stringtest
  eqtest
