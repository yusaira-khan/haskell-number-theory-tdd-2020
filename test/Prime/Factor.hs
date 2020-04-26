module Prime.Factor (spec)where
import Test.Hspec

primeFactorsOf :: Int -> [Int]
primeFactorsOf n =
  if n > 1
  then [n]
  else []

checkFactors :: Int -> [Int] -> SpecWith ()
checkFactors num factors = it ((show num)++" -> "++(show factors) )$ (primeFactorsOf num) `shouldBe` factors

spec :: Spec
spec = describe "factors" $ do
  checkFactors 1 []
  checkFactors 2 [2]
  checkFactors 3 [3]
  checkFactors 4 [2,2]
