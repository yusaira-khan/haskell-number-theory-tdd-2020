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
showTernary :: [Int]->String
showTernary [] = "0"
showTernary [1] = "1"
showTernaryWBase :: [Int] -> String
showTernaryWBase ter =
  let base = "3"
      num = showTernary ter
      in base++"_"++num
newtype STernary = STernary {sTernary :: [Int]}
instance Show STernary where
  show = H.show' sTernary (showTernaryWBase.sTernary)
instance Enum STernary where
  toEnum d = STernary $ toEnum' d
  fromEnum st = H.fromEnum' $ sTernary st
