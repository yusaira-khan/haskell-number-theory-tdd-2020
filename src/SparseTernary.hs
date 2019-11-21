module SparseTernary(toEnum') where
toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num = if num <= 3 then [num] else (toEnum' (num -3))++[3]
