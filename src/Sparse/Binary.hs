--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,mkSBinary) where
import qualified Sparse.Helper as H


newtype SBinary = SBinary {sBinary :: [Int]}
mkSBinary :: [Int] -> SBinary
mkSBinary = H.mkSparse SBinary 2

instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = mkSBinary $ H.toEnumInBase 2 d
  fromEnum sb = H.fromEnum' $ sBinary sb
  succ sb = mkSBinary $ H.addBasePow 2 1 $ sBinary sb
  pred sb = mkSBinary $ H.removeBasePow 2 1 $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = H.isEqualList (sBinary sb1) (sBinary sb2)

instance Ord SBinary where
  compare = H.compareInc sBinary

instance Num SBinary where
 (+) a b = undefined
