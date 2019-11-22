module Sparse.Ternary(toEnum') where
import Sparse.Helper as H

largestPow3SoFar :: Int -> Int
largestPow3SoFar = H.largestPowerOfBaseBetween 3 1

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
