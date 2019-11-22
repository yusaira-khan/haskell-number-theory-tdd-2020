module Sparse.Ternary(toEnum') where
import Sparse.Helper as H

largestPow3SoFar :: Int -> Int
largestPow3SoFar = H.largestPowerOfBaseBetween 3 1

toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 =  largestPow3SoFar num
      pow3Mul = (num `quot` pow3)*pow3
      rest = num `rem` pow3
  in if (num`mod`pow3==0)
    then [num]
    else (toEnum' rest)++[pow3Mul]
