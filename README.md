# number-theory-tdd
in ghci
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
*Main Lib Sparse.Binary Sparse.Helper Sparse.Ternary>
```
