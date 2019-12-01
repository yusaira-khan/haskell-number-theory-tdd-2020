module Sparse.Helper(largestPowBaseBetween,fromEnum',show',showReprWBase,toEnumInBase,isEqualList) where
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

showDigit :: Int -> String
showDigit num
  | 0<= num && num <= 9 = show num
  | 10 <= num && num <= 35 =
    let
      diff = num - 10
      startChar = fromEnum 'a'
      numChar = startChar + diff
      char  = toEnum numChar :: Char
    in [char]
  | otherwise = undefined


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
        curDigitStr = showDigit curDigit
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

toEnumInBase :: Int -> Int -> [Int]
toEnumInBase base =
  let
    largestPowSoFar = largestPowBaseBetween base 1
    toEnum' 0 = []
    toEnum' num =
      let pow =  largestPowSoFar num
          digit = (num `quot` pow)
          powMul = digit * pow
          rest = num `rem` pow
      in if (num > powMul)
        then toEnum' rest++[powMul]
        else [powMul]
  in toEnum'

isEqualList :: [Int] -> [Int] -> Bool
isEqualList [] [] = True
isEqualList [] _ = False
isEqualList _ [] = False
isEqualList (n1:r1) (n2:r2) = n1 == n2 && isEqualList r1 r2
