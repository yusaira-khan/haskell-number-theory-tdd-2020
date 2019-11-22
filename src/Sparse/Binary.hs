module Sparse.Binary(toEnum') where
largestPowerOfTwoBetween :: Int -> Int -> Int
largestPowerOfTwoBetween pow2 num = case compare pow2 num of
  GT -> (quot pow2 2)
  EQ -> pow2
  LT -> largestPowerOfTwoBetween (pow2*2) num
largestPowerOfTwoSoFar :: Int -> Int
largestPowerOfTwoSoFar = largestPowerOfTwoBetween 1
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow2 = largestPowerOfTwoSoFar num
  in if num > pow2
    then toEnum' (num -pow2)++[pow2]
    else [num] -- num == pow2
