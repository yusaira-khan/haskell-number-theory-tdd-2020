module Sparse.General(mkSTernary, mkSBinary,isSmallerInBase, SBinary, STernary) where

addList [] l = l
addList l [] = l
addList l1 l2 =
  let
    h = head l1
    p = largestPowBaseBetween 3 1 h
    r = addBasePow 3 p l2
  in r
-- addDigitToList base digit list@(least@rest) =
--   let
--     powDigit = getPow digit
--     powLeast = getPow least
-- addDigitWithCarry base digit list =
newtype STernary = STernary {sTernary :: [Int]}
mkSTernary :: [Int] -> STernary
mkSTernary = mkSparse STernary 3
instance Show STernary where
  show = show' sTernary (showReprWBase 3.sTernary)
instance Enum STernary where
  toEnum  d = STernary $ toEnumInBase 3 d
  fromEnum st = fromEnum' $ sTernary st
  succ st = mkSTernary $ addBasePow 3 1 $ sTernary st
  pred st = mkSTernary $ removeBasePow 3 1 $ sTernary st

instance Bounded STernary where
  minBound  = mkSTernary []
  maxBound = undefined
instance Eq STernary where
  (==) s1 s2 = isEqualList (sTernary s1) (sTernary s2)
instance Num STernary where
 (+) s1 s2  = mkSTernary $ addList (sTernary s1) (sTernary s2)

newtype SBinary = SBinary {sBinary :: [Int]}
mkSBinary :: [Int] -> SBinary
mkSBinary = mkSparse SBinary 2

instance Show SBinary where
  show = show' sBinary (showReprWBase 2.sBinary)
instance Enum SBinary where
  toEnum d = mkSBinary $ toEnumInBase 2 d
  fromEnum sb = fromEnum' $ sBinary sb
  succ sb = mkSBinary $ addBasePow 2 1 $ sBinary sb
  pred sb = mkSBinary $ removeBasePow 2 1 $ sBinary sb

instance Eq SBinary where
  (==) sb1 sb2 = isEqualList (sBinary sb1) (sBinary sb2)

instance Ord SBinary where
  compare = compareInc sBinary

addHelper :: [Int] -> [Int] -> [Int]
addHelper [] l = l
addHelper l [] = l
addHelper l1@(h1:t1) l2@(h2:t2) =
  case compare h1 h2 of
    LT -> h1: addHelper t1 l2
    GT -> addHelper l2 l1
    EQ -> addHelper t2 $ addHelper [h1*2] t1
instance Num SBinary where
 (+) a b = mkSBinary $ addHelper (sBinary a) (sBinary b)
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


isValidPowBase :: Int -> Int -> Bool
isValidPowBase base =let isValid num = let (q,r) = quotRem num base
                                       in if q == 0
                                       then 0 <= r && r < base
                                       else r==0 && isValid q
   in isValid
isSmallerInBase  :: Int ->  (Bool,Int) -> Int -> (Bool,Int)
isSmallerInBase base =
  let isSmaller (ok,pow) curr =
        let
          powForCurr = largestPowBaseBetween base 1 curr
          updatedOk = ok && powForCurr > pow
          updatedPow = if updatedOk then powForCurr else pow
          in  (updatedOk,updatedPow)
  in isSmaller
isRightOrderInBase :: Int -> [Int] -> Bool
isRightOrderInBase base l =
  let (ok,_) = foldl (isSmallerInBase base) (True,0) l
  in ok


mkSparse :: ([Int] -> a) -> Int ->[Int] -> a
mkSparse cons base =
  let mk l=
        let incorrectElements = filter (not.(isValidPowBase base)) l
        in if null incorrectElements
        then
          if isRightOrderInBase base l
          then cons l
          else error $ "Incorrect order " ++ show l
        else error $ "Invalid elements " ++ show incorrectElements
  in mk
data BaseDigit = None |First | Middle | Last
getBaseDigit :: Int -> Int -> Int -> BaseDigit
getBaseDigit base pow powDigit =
  let nextPow = pow *base
      lastDigit = nextPow - pow
      firstDigit = pow
  in if nextPow <= powDigit
  then None
  else if powDigit == lastDigit
  then Last
  else if powDigit == firstDigit
  then First
  else if  pow<= powDigit
  then Middle
  else undefined

addBasePow ::Int -> Int -> [Int] -> [Int]
addBasePow base =
  let
    powDigit = getBaseDigit base
    addPowList pow =
        let
          getDig = powDigit pow
          nextpow = pow*base
          handleLast list = addPowList nextpow $ tail list
          handleMiddle list =
            let
              curr = head list
              rest = tail list
            in (curr+pow):rest
          addList [] = [pow]
          addList full@(curr:_) =
            case getDig curr of
             None -> pow:full
             Last -> handleLast full
             Middle -> handleMiddle full
             First -> if base == 2
                      then handleLast full
                      else handleMiddle full
        in addList
  in addPowList

removeBasePow :: Int -> Int -> [Int] ->[Int]
removeBasePow base =
 let removePow pow =
       let
         getDig = getBaseDigit base pow
         nextPow = pow*base
         handleFirst list = tail list
         handleMiddle list =
           let rest = tail list
               curr = head list
               in (curr -pow):rest
         handleNone list = (nextPow-pow):removePow nextPow list
         predList :: [Int] -> [Int]
         predList [] = []
         predList full@(curr:_) =
           case getDig curr of
             First -> handleFirst full
             Last ->  if base==2 then handleFirst full else handleMiddle full
             Middle -> handleMiddle full
             None -> handleNone full
       in predList
 in removePow

compareDecList :: [Int] -> [Int] -> Ordering
compareDecList [] [] = EQ
compareDecList [] _ = LT
compareDecList _ [] = GT
compareDecList (h1:t1) (h2:t2) =
  case compare h1 h2 of
    EQ -> compareDecList t1 t2
    _ -> compare h1 h2

compareInc :: (a->[Int]) -> a -> a -> Ordering
compareInc getlist val1 val2 =
  let f = reverse.getlist
  in compareDecList (f val1) (f val2)
