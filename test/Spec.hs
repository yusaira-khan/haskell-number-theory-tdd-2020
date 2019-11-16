module Main where
import Test.Hspec
import Test.QuickCheck
import Lib as L
import SparseBinary as SB

emptySparse :: SpecWith ()
emptySparse = it "Empty" (shouldBe (SB.getSparseBinary 0) [])
sbTest :: IO()
sbTest = do
   hspec $ do
     describe "Sparse" $ do
       emptySparse
exampleTest :: IO()
exampleTest = do
   hspec $ do
     describe "Addition" $ do
       it "1 + 1 is greater than 1" $ do
         (1 + 1) > 1 `shouldBe` True
       it "2 + 2 is equal to 4" $ do
         2 + 2 `shouldBe` 4
       it "x + 1 is always greater than x" $ do
         property $ \x -> x + 1 > (x :: Int)
   L.someFunc

main :: IO ()
main = do
  sbTest
