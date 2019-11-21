module SparseTernary(toEnum') where
getPow3 num = 3
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 = getPow3 num
  in let pow3Mul = num `quot` pow3
    in if (num`mod`pow3==0)
      then [num]
      else (toEnum' (num -pow3Mul))++[pow3Mul]
