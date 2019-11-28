--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary(SBinary),toEnum') where
import Sparse.Helper as H

toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 2


eqlist :: [Int] -> [Int] -> Bool
eqlist [] [] = True
eqlist [] _ = False
eqlist _ [] = False
eqlist (n1:r1) (n2:r2) = n1 == n2 && eqlist r1 r2

succlist :: [Int] -> [Int]
succlist l = [1]

newtype SBinary = SBinary {sBinary :: [Int]}
instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = SBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
  succ sb = SBinary $ succlist $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = eqlist (sBinary sb1) (sBinary sb2)
