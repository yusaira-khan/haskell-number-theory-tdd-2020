module Helper(testToEnum,bindAllList,testGen,checkStrReprFun,checkSucc) where
import Test.Hspec
import qualified NumberNames as N

checkRepr ::(Int-> [Int])->(Int,[Int]) -> Expectation
checkRepr fun (num,repr) = shouldBe (fun num) repr

testToEnum ::(Int-> [Int])-> (Int,[Int]) -> SpecWith ()
testToEnum fun (num,enum) = it (N.names num)$ checkRepr fun (num,enum)
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
