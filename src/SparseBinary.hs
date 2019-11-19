module SparseBinary(toEnum') where
toEnum' :: Int -> [Int]
toEnum' num = if (num == 0) then [] else if num > 2 then (toEnum' (num -2))++[2] else [num]
