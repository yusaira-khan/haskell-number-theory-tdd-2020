module Sparse.Ternary(STernary(STernary),mkSTernary) where
import Sparse.Helper as H

addHelper l1 l2 = []
newtype STernary = STernary {sTernary :: [Int]}
mkSTernary :: [Int] -> STernary
mkSTernary = H.mkSparse STernary 3
instance Show STernary where
  show = H.show' sTernary (H.showReprWBase 3.sTernary)
instance Enum STernary where
  toEnum  d = STernary $ H.toEnumInBase 3 d
  fromEnum st = H.fromEnum' $ sTernary st
  succ st = mkSTernary $ H.addBasePow 3 1 $ sTernary st
  pred st = mkSTernary $ H.removeBasePow 3 1 $ sTernary st

instance Bounded STernary where
  minBound  = mkSTernary []
  maxBound = undefined
instance Eq STernary where
  (==) s1 s2 = H.isEqualList (sTernary s1) (sTernary s2)
instance Num STernary where
 (+) s1 s2  = mkSTernary $ addHelper (sTernary s1) (sTernary s2)
