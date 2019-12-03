module Sparse.Ternary(toEnum',STernary(STernary)) where
import Sparse.Helper as H


toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 3



newtype STernary = STernary {sTernary :: [Int]}
instance Show STernary where
  show = H.show' sTernary (H.showReprWBase 3.sTernary)
instance Enum STernary where
  toEnum d = STernary $ toEnum' d
  fromEnum st = H.fromEnum' $ sTernary st
  succ st = STernary [1]

instance Eq STernary where
  (==) s1 s2 = H.isEqualList (sTernary s1) (sTernary s2)
