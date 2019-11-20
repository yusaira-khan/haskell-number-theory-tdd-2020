module SparseBinary(toEnum') where
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num = if num > 2
  then (toEnum' (num -2))++[2]
  else [num]
