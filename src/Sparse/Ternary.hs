module Sparse.Ternary(STernary(STernary),mkSTernary) where
import Sparse.Helper as H

addList [] l = l
addList l [] = l
addList l1 l2 =
  let
    h = head l1
    p = H.largestPowBaseBetween 3 1 h
    r = H.addBasePow 3 p l2
  in r
addDigitToList base digit list@(least@rest) =
  let
    powDigit = getPow digit
    powLeast = getPow least
addDigitWithCarry base digit list =
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
 (+) s1 s2  = mkSTernary $ addList (sTernary s1) (sTernary s2)
