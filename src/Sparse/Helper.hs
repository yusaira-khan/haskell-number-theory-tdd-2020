module Sparse.Helper(largestPowBaseBetween,fromEnum',show') where
largestPowBaseBetween :: Int -> Int -> Int -> Int
largestPowBaseBetween base pow num = case compare pow num of
  GT -> (quot pow base)
  EQ -> pow
  LT -> largestPowBaseBetween base (pow*base) num

fromEnum' :: [Int] -> Int
fromEnum' = sum

show' :: (Enum a)=>(a->[Int]) -> (a->String) -> a -> String
show' sparseFun baseReprFun sb =
 let listStr = show $sparseFun sb
     decStr = show $fromEnum sb
     fullBaseStr = baseReprFun sb
 in "(S="++listStr++"|D="++decStr++"|B="++fullBaseStr++")"
