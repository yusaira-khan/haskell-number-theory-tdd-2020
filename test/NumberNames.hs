module NumberNames (names,spec)where
import Test.Hspec
nameOnes = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
nameTeens = ["Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]
nameTyList = ["Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety"]
nameTys num =
  let
    (numTy,numOne) = quotRem num 10
    namety = nameTyList !! ( numTy - 2)
  in namety ++ (nameRest numOne)
nameRest 0 = ""
nameRest num = " " ++ (names num)
nameHundred num = undefined
names :: Int -> String
names num =
  if 0 <= num && num< 10
  then nameOnes !! num
  else if 10 <= num && num < 20
  then nameTeens !! (num - 10)
  else if 20 <= num && num < 100
  then nameTys num
  else if 100 <= num && num < 100
  then nameTys num
  else undefined

checkNames :: (Int,String) -> SpecWith ()
checkNames (num,name) = it name $ (names num) `shouldBe` name

spec :: Spec
spec = describe "Testing number names" $ do
  checkNames (0,"Zero")
  checkNames (11,"Eleven")
  checkNames (20,"Twenty")
  checkNames (21,"Twenty One")
  checkNames (57,"Fifty Seven")
  -- checkNames (164,"One Hundred Sixty Four")
