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

stringtest :: Spec
stringtest = describe "Sparse Binary String" $ do
  it "Zero" $ (show(SB.SBinary(SB.toEnum' 0))) `shouldBe` "(S=[]|D=0|B=02_0)"
  it "One" $ (show(SB.SBinary(SB.toEnum' 1))) `shouldBe` "(S=[1]|D=1|B=02_1)"


spec = do
  toEnum''
  stringtest
