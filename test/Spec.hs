module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import Sparse.Ternary as ST
import qualified Sparse.BinarySpec
import qualified Sparse.TernarySpec
exampleTest :: Spec
exampleTest = do
--    hspec $ do
     describe "Addition" $ do
       it "1 + 1 is greater than 1" $ do
         (1 + 1) > 1 `shouldBe` True
       it "2 + 2 is equal to 4" $ do
         2 + 2 `shouldBe` 4
       it "x + 1 is always greater than x" $ do
         property $ \x -> x + 1 > (x :: Int)
   -- L.someFunc

main :: IO ()
main = hspec $ do
  Sparse.BinarySpec.spec
  Sparse.TernarySpec.spec
