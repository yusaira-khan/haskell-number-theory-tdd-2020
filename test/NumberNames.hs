module NumberNames (names,spec)where
import Test.Hspec
nameOnes = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
nameTens = ["Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]
names :: Int -> String
names num =
  if 0 <= num && num< 10
  then nameOnes !! num
  else if 10 <= num && num <= 20
  then nameTens !! (num - 10)
  else undefined

checkNames :: (Int,String) -> SpecWith ()
checkNames (num,name) = it name $ (names num) `shouldBe` name

spec :: Spec
spec = describe "Testing number names" $ do
  checkNames (0,"Zero")
  checkNames (11,"Eleven")
