module Main where
import Test.Hspec
import Lib as L
import Sparse.Ternary as ST
import qualified Sparse.BinarySpec
import qualified Sparse.TernarySpec

main :: IO ()
main = hspec $ do
  Sparse.BinarySpec.spec
  Sparse.TernarySpec.spec
