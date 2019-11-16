module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

main :: IO ()
main = do
  hspec $ do
    describe "Addition" $ do
      it "1 + 1 is greater than 1" $ do
        (1 + 1) > 1 `shouldBe` True
      it "2 + 2 is equal to 4" $ do
        2 + 2 `shouldBe` 4
      it "x + 1 is always greater than x" $ do
        property $ \x -> x + 1 > (x :: Int)
  L.someFunc
