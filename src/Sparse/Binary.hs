--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,asBigAs,mkSBinary,toEnum') where
import Sparse.Helper as H

toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 2


eqlist :: [Int] -> [Int] -> Bool
eqlist [] [] = True
eqlist [] _ = False
eqlist _ [] = False
eqlist (n1:r1) (n2:r2) = n1 == n2 && eqlist r1 r2

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
isValidPow2 :: Int -> Bool
isValidPow2 n =
  let (q,r) = quotRem n 2
  in if q == 0
  then 0 <= r && r < 2
  else r==0 && isValidPow2 q
asBigAs  :: (Bool,Int) -> Int -> (Bool,Int)
asBigAs (ok,pow2) curr=
  let newok = ok && curr > pow2
      newpow2 = if newok then curr else pow2
  in  (newok,newpow2)

inRightOrder :: [Int] -> Bool
inRightOrder  l= let (ok,_) = foldl asBigAs (True,0) l in ok

pred2List :: [Int] -> [Int]
pred2List [] = undefined 
pred2List (1:l) = l
pred2List (n:l) = 1:l
newtype SBinary = SBinary {sBinary :: [Int]}
mkSBinary :: [Int] -> SBinary
mkSBinary [] =  SBinary []
mkSBinary l=
  let incorrectElements = filter (not.isValidPow2) l
  in if null incorrectElements
  then
    if inRightOrder l
    then SBinary l
    else error $ "Incorrect order " ++ show l
  else error $ "Invalid elements " ++ show incorrectElements

instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = SBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
  succ sb = SBinary $ addPow2List 1 $ sBinary sb
  pred sb = SBinary $ pred2List $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = eqlist (sBinary sb1) (sBinary sb2)
