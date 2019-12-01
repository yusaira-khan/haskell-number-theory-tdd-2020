module Main where
import Test.Hspec
import qualified Sparse.BinarySpec
import qualified Sparse.TernarySpec
import qualified Sparse.HelperSpec

main :: IO ()
main = hspec $ do
  Sparse.BinarySpec.spec
  Sparse.TernarySpec.spec
  Sparse.HelperSpec.spec
