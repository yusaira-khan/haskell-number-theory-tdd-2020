--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,mkSBinary) where
import qualified Sparse.Helper as H

compareRevList :: [Int] -> [Int] -> Ordering
compareRevList [] [] = EQ
compareRevList [] _ = LT
compareRevList _ [] = GT
compareRevList (h1:t1) (h2:t2) =
  case compare h1 h2 of
    EQ -> compareRevList t1 t2
    _ -> compare h1 h2


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
  compare sb1 sb2 =
    let f = reverse.sBinary
    in compareRevList (f sb1) (f sb2)
