module Sparse.Binary(toEnum') where
import Sparse.Helper as H
largestPowerOfTwoSoFar :: Int -> Int
largestPowerOfTwoSoFar = H.largestPowerOfBaseBetween 2 1
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow2 = largestPowerOfTwoSoFar num
  in if num > pow2
    then toEnum' (num -pow2)++[pow2]
    else [num] -- num == pow2
