module SparseTernary(toEnum') where
getPow3 num = 3
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow3 = getPow3 num
  in if ((num <= pow3) || (num`mod`pow3==0))
    then [num]
    else (toEnum' (num -pow3))++[pow3]
