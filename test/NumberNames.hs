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
nameThousand num =
  let
    (numThousand,numHundred) = quotRem num 1000
    nameT = names numThousand
    nameR = nameRest numHundred
  in nameT ++ " Thousand" ++nameR
nameHundred num =
  let
    (numHundred,numTens) = quotRem num 100
    nameH = names numHundred
    nameR = nameRest numTens
  in nameH ++ " Hundred" ++nameR
names :: Int -> String
names num =
  if 0 <= num && num< 10
  then nameOnes !! num
  else if 10 <= num && num < 20
  then nameTeens !! (num - 10)
  else if 20 <= num && num < 100
  then nameTys num
  else if 100 <= num && num < 1000
  then nameHundred num
  else if 1000 <= num && num < 1000000
  then nameThousand num
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
  checkNames (164,"One Hundred Sixty Four")
  checkNames (100,"One Hundred")
  checkNames (12000,"Twelve Thousand")
  checkNames (918703,"Nine Hundred Eighteen Thousand Seven Hundred Three")
