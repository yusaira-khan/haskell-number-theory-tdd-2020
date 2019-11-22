module SparseTernary(toEnum') where

getLargestPow3Between :: Int -> Int -> Int
getLargestPow3Between pow3 high =
  case compare pow3 high of
    GT -> pow3 `quot` 3
    EQ -> pow3
    LT -> getLargestPow3Between (pow3*3) high

getLargestPow3LessThan :: Int-> Int
getLargestPow3LessThan = getLargestPow3Between 1

toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 =  getLargestPow3LessThan num
      pow3Mul = (num `quot` pow3)*pow3
      rest = num `rem` pow3
  in if (num`mod`pow3==0)
    then [num]
    else (toEnum' rest)++[pow3Mul]
