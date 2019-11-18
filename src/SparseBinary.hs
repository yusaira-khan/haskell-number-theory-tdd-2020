module SparseBinary(getSparseBinary) where
getSparseBinary :: Int -> [Int]
getSparseBinary num = if (num == 0) then [] else [num]
