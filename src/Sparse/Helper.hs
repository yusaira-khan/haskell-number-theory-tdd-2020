module Sparse.Helper(largestPowBaseBetween) where
largestPowBaseBetween :: Int -> Int -> Int -> Int
largestPowBaseBetween base pow num = case compare pow num of
  GT -> (quot pow base)
  EQ -> pow
  LT -> largestPowBaseBetween base (pow*base) num
