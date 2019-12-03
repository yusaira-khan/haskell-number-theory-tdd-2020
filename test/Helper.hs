module Helper(testToEnum,bindAllList,testGen,checkStrReprFun,checkSucc) where
import Test.Hspec

checkRepr ::(Int-> [Int])->(Int,[Int]) -> Expectation
checkRepr fun (num,repr) = shouldBe (fun num) repr

testToEnum ::(Int-> [Int])-> (String,Int,[Int]) -> SpecWith ()
testToEnum fun (name,num,enum) = it name $ checkRepr fun (num,enum)
bindAllList :: (Monad m) => (a -> m b) -> [a] -> m b
bindAllList fun (a:[]) = fun a
bindAllList fun (a:as) = fun a >>  bindAllList fun as
testGen :: (a->SpecWith()) -> String -> [a] -> Spec
testGen fun name list = describe name $ bindAllList fun list

checkStrReprFun ::(Int->String)->(String,Int,String) -> SpecWith ()
checkStrReprFun fun (name,num,rep) =
  it name $ (fun num) `shouldBe` rep

checkSucc ::(Enum a, Show a,Eq a)=>(Int-> a)->(String,Int) -> SpecWith ()
checkSucc s (name,num)= it name $ (succ (s num)) `shouldBe` (s (succ num))
