# number-theory-tdd
in ghci
## Binary
```haskell
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb n = toEnum n :: Sparse.Binary.SBinary
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 50
(S=[2,16,32]|D=50|B=2_110010)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 32
(S=[32]|D=32|B=2_100000)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 16
(S=[16]|D=16|B=2_10000)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 8
(S=[8]|D=8|B=2_1000)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 4
(S=[4]|D=4|B=2_100)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 3
(S=[1,2]|D=3|B=2_11)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 15
(S=[1,2,4,8]|D=15|B=2_1111)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 100
(S=[4,32,64]|D=100|B=2_1100100)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> sb 243
(S=[1,2,16,32,64,128]|D=243|B=2_11110011)
```
## Ternary
```haskell
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st n = toEnum n :: Sparse.Ternary.STernary
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 3
(S=[3]|D=3|B=3_10)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 30
(S=[3,27]|D=30|B=3_1010)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 10
(S=[1,9]|D=10|B=3_101)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 256
(S=[1,3,9,243]|D=256|B=3_100111)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 50
(S=[2,3,18,27]|D=50|B=3_1212)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 34
(S=[1,6,27]|D=34|B=3_1021)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 729
(S=[729]|D=729|B=3_1000000)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 100
(S=[1,18,81]|D=100|B=3_10201)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 27
(S=[27]|D=27|B=3_1000)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 9
(S=[9]|D=9|B=3_100)
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> st 16
(S=[1,6,9]|D=16|B=3_121)
```
## Helper
```
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> Sparse.Helper.showReprWBase 16 [10]
"16_a"
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> Sparse.Helper.showReprWBase 16 [16]
"16_10"
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary> Sparse.Helper.showReprWBase 16 [10,16]
"16_1a"
```
