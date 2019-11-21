module SparseBinary(toEnum') where
largestPowerOfTwoSoFar :: Int -> Int
largestPowerOfTwoSoFar num =
  if num >= 4
  then 4
  else if num >= 2
    then 2
    else 1
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow2 = largestPowerOfTwoSoFar num
  in if num > pow2
    then toEnum' (num -pow2)++[pow2]
    else [num] -- num == pow2
