module Sparse.Helper(largestPowBaseBetween,fromEnum',show',showReprWBase) where
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

showReprInBase :: Int -> [Int]->String
showReprInBase base =
  let
    showReprGreaterThanPow   1 [] = "0"
    showReprGreaterThanPow _ [] = ""
    showReprGreaterThanPow pow full@(num:rest) =
      let
        nextPow = pow * base
        curDigit =
          if num >= nextPow
          then 0
          else num `quot` pow
        curDigitStr = show curDigit
        higherDigits =
          if num >= nextPow
          then full
          else rest
        higherDigitStr = showReprGreaterThanPow nextPow higherDigits
      in higherDigitStr ++ curDigitStr
    in showReprGreaterThanPow 1


showReprWBase :: Int -> [Int] -> String
showReprWBase base numlist =
  let baseStr =  show base
      num = showReprInBase base numlist
      in baseStr++"_"++num
