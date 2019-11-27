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
checkStrReprFun ::(Int->String)->(String,Int,String) -> SpecWith ()
checkStrReprFun fun (name,num,rep) =
  it name $ (fun num) `shouldBe` rep
checkStrRepr ::(String,Int,String) -> SpecWith ()
checkStrRepr  = checkStrReprFun (show . SB.SBinary . SB.toEnum')
stringtest :: Spec
stringtest = describe "Sparse Binary String" $ do
  checkStrRepr ("Zero",0,"(S=[]|D=0|B=2_0)")
  checkStrRepr ("One",1,"(S=[1]|D=1|B=2_1)")
  checkStrRepr ("Two",2,"(S=[2]|D=2|B=2_10)")
  checkStrRepr ("Three",3,"(S=[1,2]|D=3|B=2_11)")
  checkStrRepr ("Four",4,"(S=[4]|D=4|B=2_100)")


spec = do
  toEnum''
  stringtest
