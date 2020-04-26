module Prime.Factor (spec)where
import Test.Hspec

primeFactorsOf :: Int -> [Int]
primeFactorsOf n =
  let factors = if n >1 then [2] else [] 
  in factors

checkFactors :: Int -> [Int] -> SpecWith ()
checkFactors num factors = it ((show num)++" -> "++(show factors) )$ (primeFactorsOf num) `shouldBe` factors

spec :: Spec
spec = describe "factors" $ do
  checkFactors 1 []
  checkFactors 2 [2]
