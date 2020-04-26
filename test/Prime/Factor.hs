module Prime.Factor (spec)where
import Test.Hspec

factor :: Int -> [Int]
factor _ = []

checkFactors :: Int -> [Int] -> SpecWith ()
checkFactors num factors = it ((show num)++" -> "++(show factors) )$ (factor num) `shouldBe` factors

spec :: Spec
spec = describe "factors" $ do
  checkFactors 1 []
