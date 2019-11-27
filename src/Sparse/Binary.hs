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

newtype SBinary = SBinary {sBinary :: [Int]}

showBinListAsBase :: Int -> [Int] -> String
showBinListAsBase 1 [] = "0"
showBinListAsBase _ [] = ""
showBinListAsBase pow2 full@(num:rest )=
  let
    bitPresent = num==pow2
    leastSigBit = if bitPresent then "1" else "0"
    higherBitList = if bitPresent then rest else full
    higherBits = (showBinListAsBase (pow2*2) higherBitList)
  in higherBits++leastSigBit

showBinaryWBase :: SBinary -> String
showBinaryWBase sb =
  let baseStr = "2"
      sl = sBinary sb
  in baseStr++"_"++ (showBinListAsBase 1 sl)

instance Show SBinary where
  show = H.show' sBinary showBinaryWBase
instance Enum SBinary where
  toEnum d = SBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
