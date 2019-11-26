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
show' :: (Enum a)=>(a->[Int]) -> (a->String) -> a -> String
show' sparseFun binaryReprFun sb =
 let listStr = show $sparseFun sb
     decStr = show $fromEnum sb
     fullBaseStr = binaryReprFun sb
 in "(S="++listStr++"|D="++decStr++"|B="++fullBaseStr++")"

newtype SBinary = SBinary {sBinary :: [Int]}

showBinary :: SBinary -> String
showBinary sb =
  let baseStr = "2"
      inBaseStr = show $fromEnum sb
  in baseStr++"_"++inBaseStr

instance Show SBinary where
  show = show' sBinary showBinary
instance Enum SBinary where
  toEnum d = SBinary $ toEnum' d
  fromEnum sb = fromEnum' $ sBinary sb