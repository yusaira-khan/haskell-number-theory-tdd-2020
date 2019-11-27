module Sparse.Ternary(toEnum',STernary(STernary)) where
import Sparse.Helper as H

largestPow3SoFar :: Int -> Int
largestPow3SoFar = H.largestPowBaseBetween 3 1

toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 =  largestPow3SoFar num
      digit = (num `quot` pow3)
      pow3Mul = digit * pow3
      rest = num `rem` pow3
  in if (num > pow3Mul)
    then toEnum' rest++[pow3Mul]
    else [num] -- num == pow3Mul



newtype STernary = STernary {sTernary :: [Int]}
instance Show STernary where
  show = H.show' sTernary (H.showReprWBase 3.sTernary)
instance Enum STernary where
  toEnum d = STernary $ toEnum' d
  fromEnum st = H.fromEnum' $ sTernary st
