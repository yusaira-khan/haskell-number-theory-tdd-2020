module Sparse.GeneralSpec(spec) where
import Test.Hspec
import Sparse.General as SH
checkAsBig :: (String,(Bool,Int),Int,(Bool,Int)) -> SpecWith ()
checkAsBig (s,t1,n,t2)= it s $ (SH.isSmallerInBase 2 t1 n) `shouldBe` t2
checkBig3 :: (String,(Bool,Int),Int,(Bool,Int)) -> SpecWith ()
checkBig3 (s,t1,n,t2)= it s $ (SH.isSmallerInBase 3 t1 n) `shouldBe` t2
asBigAsTest  :: Spec
asBigAsTest  = describe "as big as" $ do
  checkAsBig ("simplest",(True, 0),1,(True,1))
  checkAsBig ("False",(False, 1),2,(False,1))
  checkAsBig ("Less2",(True, 1),2,(True,2))
  checkAsBig ("Less4",(True, 1),4,(True,4))
  checkAsBig ("Equal",(True, 1),1,(False,1))
  checkAsBig ("Greater",(True, 2),1,(False,2))
big3test :: Spec
big3test  = describe  "big 3" $ do
  checkBig3 ("simplest",(True,0),1,(True,1))
  checkBig3 ("Less3",(True, 1),3,(True,3))
  checkBig3 ("Flase",(False, 1),3,(False,1))
  checkBig3 ("Equal",(True, 1),1,(False,1))
  checkBig3 ("EqualInPow",(True,1),2,(False,1))
  checkBig3 ("Greater",(True, 3),1,(False,3))
  checkBig3 ("Less18",(True, 1),18,(True,9))
spec = do
  asBigAsTest
  big3test
