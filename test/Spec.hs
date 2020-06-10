module Main where
import Test.Hspec
import qualified Sparse.BinarySpec
import qualified Sparse.TernarySpec
import qualified Sparse.GeneralSpec
import qualified NumberNames
import qualified Prime.Factor

main :: IO ()
main = hspec $ do
  Sparse.BinarySpec.spec
  Sparse.TernarySpec.spec
  Sparse.GeneralSpec.spec
  NumberNames.spec
  Prime.Factor.spec
