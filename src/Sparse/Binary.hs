--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,mkSBinary) where
import qualified Sparse.Helper as H

compareDecList :: [Int] -> [Int] -> Ordering
compareDecList [] [] = EQ
compareDecList [] _ = LT
compareDecList _ [] = GT
compareDecList (h1:t1) (h2:t2) =
  case compare h1 h2 of
    EQ -> compareDecList t1 t2
    _ -> compare h1 h2

compareInc :: (a->[Int]) -> a -> a -> Ordering
compareInc getlist val1 val2 =
  let f = reverse.getlist
  in compareDecList (f val1) (f val2)

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
  compare = compareInc sBinary
