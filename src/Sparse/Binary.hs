--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary(SBinary),toEnum') where
import Sparse.Helper as H

largestPow2SoFar :: Int -> Int
largestPow2SoFar = H.largestPowBaseBetween 2 1

toEnum' :: Int -> [Int]
toEnum' 0 = []
toEnum' num =
  let pow2 = largestPow2SoFar num
      rest = num `rem` pow2
  in if num > pow2
    then toEnum' rest++[pow2]
    else [num] -- num == pow2

fromEnum' :: [Int] -> Int
fromEnum' = sum

newtype SBinary = SBinary {sBinary :: [Int]}

instance Show SBinary where
  show sb = let liststr = show $sBinary sb
    in "(SB=[]|D=0|B=02_0)"
