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

addHelper :: [Int] -> [Int] -> [Int]
addHelper [] l = l
addHelper l [] = l
addHelper l1@(h1:t1) l2@(h2:_) =
  case compare h1 h2 of
    LT -> h1: addHelper t1 l2
    GT -> addHelper l2 l1
    EQ -> [h1*2]
instance Num SBinary where
 (+) a b = mkSBinary $ addHelper (sBinary a) (sBinary b)
