--{-# LANGUAGE InstanceSigs #-}
module Sparse.Binary(SBinary,mkSBinary,toEnum') where
import qualified Sparse.Helper as H

toEnum' :: Int -> [Int]
toEnum' = H.toEnumInBase 2


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
newtype SBinary = SBinary {sBinary :: [Int]}
mkSBinary :: [Int] -> SBinary
mkSBinary = H.mkSparse SBinary 2

instance Show SBinary where
  show = H.show' sBinary (H.showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = mkSBinary $ toEnum' d
  fromEnum sb = H.fromEnum' $ sBinary sb
  succ sb = mkSBinary $ H.addBasePow 2 1 $ sBinary sb
  pred sb = mkSBinary $ removePowFromList 1 $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = H.isEqualList (sBinary sb1) (sBinary sb2)
