module ParmMatrixTests ( parmMatrixTests ) where

-- This file tests the construction of the BMMC corresponding to a parm mask.
-- We check that applying the parm combinator is the same as 
-- first permuting using the corresponding matrix, applying two, 
-- and then permuting using the inverse matrix.


import Test.Tasty
import Test.Tasty.QuickCheck hiding ( (.&.) )
import qualified Perm as P
import qualified Bmmc as B
import Data.Bits
import Data.List ( sort )


parmMatrixTests :: TestTree
parmMatrixTests = localOption (QuickCheckTests 10000) $ testGroup "Parm Matrix Tests" 
  [ testProperty "parm-matrix-inv" parmMatrixInvProp
  , testProperty "parm-matrix-correct" $ 
      forAllShrink (choose (0, 10)) shrink $ \n -> 
        parmMatrixCorrectProp n
  ]

parmMatrixInvProp :: Int -> Integer -> Property
parmMatrixInvProp n mask = 
  0 < n ==> 0 < mask ==> mask < 2^n ==> 
    B.inverse (parmMatrix n mask) == Just (parmInvMatrix n mask)

parmMatrixCorrectProp :: Int -> Integer -> Property 
parmMatrixCorrectProp n mask = 
  0 < n ==> n <= 10 ==> 0 < mask ==> mask < 2^n ==> 
    forAllShrink (choose (0, 2^(n-1)-1)) shrink $ \k ->
      parm mask (replicateKth k) xs == parmWithBmmc mask (replicateKth k) xs
  where xs = [0..2^n - 1]

replicateKth :: Ord a => Int -> [a] -> [a]
replicateKth k xs = replicate (length xs) (xs !! k)


-- Seperate a list in two.
-- The list of booleans tells whether each element should go left or right.
seperate :: [Bool] -> [a] -> ([a], [a])
seperate [] [] = ([], [])
seperate (b:bs) (x:xs) = if b then (ls', x:rs') else (x:ls', rs')
  where (ls', rs') = seperate bs xs

-- Merge two lists into one. 
-- The list of booleans tells whether each element should be taken from left or right 
merge :: [Bool] -> [a] -> [a] -> [a]
merge [] [] [] = []
merge (False:bs) (l:ls) rs = l : merge bs ls rs
merge (True:bs) ls (r:rs) = r : merge bs ls rs

-- The parm combinator.
parm :: Integer -> ([a] -> [a]) -> ([a] -> [a])
parm mask _ _ | mask <= 0 = undefined
parm mask f xs = merge groups (f ls) (f rs)
  where (ls, rs) = seperate groups xs
        groups = [ odd $ popCount (mask .&. toInteger i) `mod` 2 | i <- [0..length xs - 1] ]
        
-- The two combinator. 
two :: ([a] -> [a]) -> ([a] -> [a])
two f xs = f ls ++ f rs
  where ls = take (length xs `div` 2) xs
        rs = drop (length xs `div` 2) xs

-- Make a k-regular column out of two binary functions
--col :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
--col k f g xs = [ res i | i <- [0..length xs - 1] ]
--  where res i = if i < (i `xor` k) 
--                then f (xs !! i) (xs !! (i `xor` k))
--                else g (xs !! i) (xs !! (i `xor` k))

--removeBit :: Int -> Int -> Int 
--removeBit i x = ((x .&. high) `shiftR` 1) .|. (x .&. low)
--  where high = (complement 0) `shiftL` (i+1)
--        low  = (1 `shiftL` i) - 1

-- Make a k-regular column, but build it using parm.
--colWithParm :: Int -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [a]
--colWithParm k _ _ _ | k <= 0 = undefined
--colWithParm k f g [x0, x1] = [f x0 x1, g x0 x1]
--colWithParm k f g xs
--  | k `mod` 4 == 0 = ilv (col (removeBit 0 k) f g) xs
--  | k `mod` 4 == 1 = que (col (removeBit 1 k) f g) xs
--  | k `mod` 4 == 2 = ilv (col (removeBit 0 k) f g) xs
--  | k `mod` 4 == 3 = vee (col (removeBit 0 k) f g) xs

permuteBMMC :: B.BMatrix -> [a] -> [a]
permuteBMMC a xs = map (\i -> xs !! src i) [0..length xs - 1]
  where src i = fromInteger $ B.unsafeInverse a `B.transformInt` toInteger i

-- The index of the least significant bit in a positive integer
lsb :: Integer -> Int
lsb x 
  | x <= 0    = error "lsb: needs a positive argument"
  | odd x     = 0
  | otherwise = 1 + lsb (x `div` 2)

-- Get the i-th bit from an integer
getBit :: Integer -> Int -> Bool
getBit x i = odd (x `shiftR` i)
 
-- Calculate the matrix corresponding to a parm mask.
parmMatrix :: Int -> Integer -> B.BMatrix
parmMatrix n mask = B.make n n idxfun
  where idxfun i j 
          | i < lsb mask             = j == i
          | lsb mask <= i && i < n-1 = j == i + 1
          | i == n-1                 = mask `getBit` j
        
-- Calculate the inverse matrix corresponding to a parm mask.
parmInvMatrix :: Int -> Integer -> B.BMatrix
parmInvMatrix n mask = B.make n n idxfun
  where idxfun i j
          | i < lsb mask  = j == i
          | i == lsb mask = mask' `getBit` j
          | i > lsb mask  = j == i-1
        mask' = B.colToInt $ B.makeCol n maskFun'
        maskFun' i 
          | i < lsb mask             = mask `getBit` i
          | lsb mask <= i && i < n-1 = mask `getBit` (i+1)
          | i == n-1                 = True

-- The parm combinator, implemented using BMMCs and 'two'.
parmWithBmmc :: Integer -> ([a] -> [a]) -> ([a] -> [a])
parmWithBmmc mask _ xs | mask <= 0 = undefined
parmWithBmmc mask f xs = permuteBMMC (B.unsafeInverse a) $ two f $ permuteBMMC a xs
  where a = parmMatrix n mask
        n = log2 $ length xs
        log2 1 = 0
        log2 i = 1 + log2 (i `div` 2)

--parmNestToMatrix :: Int -> [Int] -> B.BMatrix
--parmNestToMatrix n [] = B.identity n
--parmNestToMatrix n (m:ms) = 
--  B.blockDiag [parmNestToMatrix (n-1) ms, B.identity 1] `B.mult` parmToMatrix n m 
--
---- Get the BMMC matrix corresponding to a column
--colToMatrix :: Int -> Int -> B.BMatrix
--colToMatrix n k | k <= 0 || n <= 0 || k >= 2^n = undefined
--colToMatrix 1 k = B.identity 1
--colToMatrix n k = innerM `B.mult` outerM 
--  where outerM = parmToMatrix n mask
--        innerM = B.blockDiag [colToMatrix (n-1) newK, B.identity 1]
--        mask
--          | k `mod` 4 == 0 = 1
--          | k `mod` 4 == 1 = 2
--          | k `mod` 4 == 2 = 1
--          | k `mod` 4 == 3 = 3
--        newK
--          | k `mod` 4 == 1 = removeBit 1 k
--          | otherwise      = removeBit 0 k


