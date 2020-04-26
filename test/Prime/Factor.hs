module Prime.Factor (spec)where
import Test.Hspec

primeFactorsOf :: Int -> [Int]
primeFactorsOf n =
  if n > 1
  then
    if (n `mod` 2)==0
    then 2:primeFactorsOf (n `div` 2)
    else
      if (n `mod` 3)==0
      then 3:primeFactorsOf (n `div` 3)
      else [n]
  else []

checkFactors :: Int -> [Int] -> SpecWith ()
checkFactors num factors = it ((show num)++" -> "++(show factors) )$ (primeFactorsOf num) `shouldBe` factors

spec :: Spec
spec = describe "factors" $ do
  checkFactors 1 []
  checkFactors 2 [2]
  checkFactors 3 [3]
  checkFactors 4 [2,2]
  checkFactors 5 [5]
  checkFactors 6 [2,3]
  checkFactors 7 [7]
  checkFactors 8 [2,2,2]
  checkFactors 9 [3,3]
