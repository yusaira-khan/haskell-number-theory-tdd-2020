module NumberNames (names,spec)where
import Test.Hspec
ones = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight","Nine"]
teens = ["Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen"]
tys = ["Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety"]
nameList list accessorFun num = list !! (accessorFun num)
nameRest 0 = ""
nameRest num = " " ++ (names num)
nameGreaterThan unit nameExt num=
  let
    (numGreater,numRest) = quotRem num unit
    nameGreater = (names numGreater) ++ " " ++  nameExt
    nameR = nameRest numRest
  in nameGreater ++ nameR
numCond (rangeStart, rangeEnd) num = rangeStart <= num && num < rangeEnd
names :: Int -> String
names num =
  if numCond (0,10) num
  then nameList ones id num
  else if numCond (10,20) num
  then nameList teens (\n -> (n - 10)) num
  else if numCond (20,100) num
  then (nameList tys (\n -> (n `quot` 10)-2) num) ++ nameRest (num `rem` 10)
  else if numCond (100,1000) num
  then nameGreaterThan 100 "Hundred" num
  else if numCond (1000,1000000) num
  then nameGreaterThan 1000 "Thousand" num
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
