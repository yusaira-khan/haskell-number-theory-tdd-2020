module Prime.Factor (spec)where
import Test.Hspec

primeFactorsStarting :: Int -> Int -> [Int]
primeFactorsStarting d n =
  if n <= 1
  then []
  else
    if (n `mod` d)==0
    then d:primeFactorsStarting d (n `div` d)
    else primeFactorsStarting (d+1) n
primeFactorsOf :: Int -> [Int]
primeFactorsOf = primeFactorsStarting 2
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
  checkFactors 10 [2,5]
  checkFactors 11 [11]
  checkFactors 12 [2,2,3]
  checkFactors 45 [3,3,5]
  checkFactors 175 [5,5,7]
