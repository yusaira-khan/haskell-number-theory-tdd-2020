module Helper(testToEnum,bindAllList,testGen,checkStrReprFun,checkSucc,checkEq,checkError,checkPred,checkComp,selfzip,checkAdd) where
import Test.Hspec
import Control.Exception(evaluate)
import qualified NumberNames as N


testToEnum :: (Show a,Eq a) => ([Int]->a)->(Int-> a)-> (Int,[Int]) -> SpecWith ()
testToEnum cons fun (num,repr) = it (N.names num)$ shouldBe (fun num) (cons repr)
bindAllList :: (Monad m) => (a -> m b) -> [a] -> m b
bindAllList fun (a:[]) = fun a
bindAllList fun (a:as) = fun a >>  bindAllList fun as
testGen :: (a->SpecWith()) -> String -> [a] -> Spec
testGen fun name list = describe name $ bindAllList fun list

checkStrReprFun ::(Int->String)->(Int,String) -> SpecWith ()
checkStrReprFun fun (num,rep) =
  it (N.names num)$ (fun num) `shouldBe` rep

checkSucc ::(Enum a, Show a,Eq a)=>(Int-> a)->Int -> SpecWith ()
checkSucc s num= it (N.names num) $ (succ (s num)) `shouldBe` (s (succ num))

checkEq :: (Enum a, Show a,Eq a)=>(Int-> a)->(Int,Int,Bool) -> SpecWith ()
checkEq s (num1,num2,truth) = it ((N.names num1)++(N.names num2)) $ ((s num1) == (s num2)) `shouldBe` truth

checkError :: (a->b)->(a,String) -> SpecWith ()
checkError fun (value, errorName) =
  it errorName $ (evaluate $ fun value) `shouldThrow` errorCall errorName

checkPred ::(Enum a, Show a,Eq a)=>(Int-> a)->(Int) -> SpecWith ()
checkPred s num = it (N.names num) $ (pred (s num)) `shouldBe` (s (pred num))

checkComp :: (Ord a)=>(Int->a)->(Int,Int) -> SpecWith()
checkComp cons (num1,num2) =
  let
    name1 = N.names num1
    name2 = N.names num2
    s1 = cons num1
    s2 = cons num2
    ordNum = compare num1 num2
    ordname = show ordNum
    ordS = compare s1 s2
    testname = name1 ++name2 ++ ordname
  in it testname $ ordS `shouldBe` ordNum


selfzip :: [Int] -> [(Int,Int)]
selfzip xlist =
  xlist >>= \x ->
  xlist >>= \xAgain ->
  return (x,xAgain)


checkAdd :: (Num a, Show a,Eq a)=>(Int->a)->(Int,Int) -> SpecWith ()
checkAdd s (num1,num2) =
  let
    name1 = N.names num1
    name2 = N.names num2
    numSum = num1 + num2
    nameSum = N.names numSum
    s1 = s num1
    s2 = s num2
    sumExpected = s numSum
    sumCalculated = s1 + s2
    testName = name1++" Plus "++name2 ++" Equals " ++nameSum
    in it testName $ sumCalculated `shouldBe` sumExpected
