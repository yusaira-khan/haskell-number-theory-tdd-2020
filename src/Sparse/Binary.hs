--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,mkSBinary,toEnum') where
import Sparse.Helper as H

toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 2

addPow2List :: Int -> [Int] -> [Int]
addPow2List pow2 =
  let
    nextpow2 = pow2*2
    addList [] = [pow2]
    addList full@(curr:rest) =
      case compare curr pow2 of
       GT -> pow2:full
       EQ -> addPow2List nextpow2 rest
       LT -> curr:(addPow2List pow2 rest)
  in addList

removePowFromList :: Int -> [Int] -> [Int]
removePowFromList pow2 =
  let
    nextPow2 = pow2*2
    pred2List :: [Int] -> [Int]
    pred2List [] = []
    pred2List full@(curr:rest) =
      case compare curr pow2 of
        EQ -> rest
        GT -> pow2:removePowFromList nextPow2 full
        LT ->  undefined
  in pred2List
isValidPow2 = H.isValidPowBase 2
newtype SBinary = SBinary {sBinary :: [Int]}
mkSBinary :: [Int] -> SBinary
mkSBinary [] =  SBinary []
mkSBinary l=
  let incorrectElements = filter (not.isValidPow2) l
  in if null incorrectElements
  then
    if H.isRightOrderInBase 2 l
    then SBinary l
    else error $ "Incorrect order " ++ show l
  else error $ "Invalid elements " ++ show incorrectElements

instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = mkSBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
  succ sb = mkSBinary $ addPow2List 1 $ sBinary sb
  pred sb = mkSBinary $ removePowFromList 1 $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = H.isEqualList (sBinary sb1) (sBinary sb2)
