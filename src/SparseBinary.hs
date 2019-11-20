module SparseBinary(toEnum') where
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  if num > 4
  then toEnum' (num -4)++[4]
  else if num == 4
    then [num]
    else if num > 2
      then toEnum' (num -2)++[2]
      else [num]
