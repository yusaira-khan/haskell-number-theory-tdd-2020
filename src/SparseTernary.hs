module SparseTernary(toEnum') where
getPow3 :: Int-> Int
getPow3 num = if num < 3 then 1 else 3
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 = getPow3 num
      pow3Mul = (num `quot` pow3)*pow3
      rest = num `rem` pow3
  in if (num`mod`pow3==0)
    then [num]
    else (toEnum' rest)++[pow3Mul]
