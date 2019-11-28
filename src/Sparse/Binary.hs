--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary(SBinary),toEnum') where
import Sparse.Helper as H

toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 2


eqlist :: [Int] -> [Int] -> Bool
eqlist [] [] = True
eqlist [] _ = False


newtype SBinary = SBinary {sBinary :: [Int]}
instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = SBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
instance Eq SBinary where
  (==) sb1 sb2 = eqlist (sBinary sb1) (sBinary sb2)
