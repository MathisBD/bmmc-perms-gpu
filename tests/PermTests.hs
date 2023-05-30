module PermTests ( permTests ) where 

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Perm as P
import qualified Bmmc as B
import Data.List ( sort )


permTests :: TestTree
permTests = testGroup "Permutation Tests" 
  [ testProperty "to-from-list" toFromListProp
  , testProperty "id-valid" idValidProp
  , testProperty "rev-valid" revValidProp
  , testProperty "swap-valid" $ 
      forAllShrink arbitrary shrink $ \n -> 
      forAllShrink (choose (0, n-1)) shrink $ \i ->
      forAllShrink (choose (0, n-1)) shrink $ \j ->
        swapValidProp n i j
  , testProperty "compose-valid" composeValidProp
  , testProperty "inv-valid" invValidProp
  , testProperty "inv-inv" invInvProp
  , testProperty "to-matrix-permute" toMatrixPermuteProp
  ]

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation xs ys = sort xs == sort ys

isPermValid :: Int -> P.Perm -> Bool
isPermValid n_expected perm = 
  n == n_expected && P.toList perm `isPermutation` [0..n-1]
  where n = P.size perm


toFromListProp :: P.Perm -> Property 
toFromListProp perm = P.fromList (P.toList perm) === perm

idValidProp :: Int -> Property
idValidProp n = n >= 0 ==> isPermValid n (P.identity n)

revValidProp :: Int -> Property
revValidProp n = n >= 0 ==> isPermValid n (P.reverse n)

swapValidProp :: Int -> Int -> Int -> Property
swapValidProp n i j = 
  n >= 0 ==> 0 <= i ==> 0 <= j ==> i < n ==> j < n ==> 
    isPermValid n (P.swap n i j)

composeValidProp :: P.Perm -> P.Perm -> Property
composeValidProp p1 p2 = 
  P.size p1 == P.size p2 ==> 
    isPermValid n $ p1 `P.compose` p2
  where n = P.size p1

invValidProp :: P.Perm -> Property
invValidProp perm = property $ isPermValid n $ P.inverse perm
  where n = P.size perm

invInvProp :: P.Perm -> Property
invInvProp perm = P.inverse (P.inverse perm) === perm

permuteProp :: P.Perm -> [Int] -> Property
permuteProp perm xs = property $ P.permute perm xs `isPermutation` xs

-- LSB is first
fromBits :: [Bool] -> Integer
fromBits [] = 0
fromBits (True:bs) = 1 + 2 * fromBits bs
fromBits (False:bs) = 2 * fromBits bs

-- LSB is first
toBits :: Int -> Integer -> [Bool]
toBits 0 x = []
toBits n x = odd x : toBits (n-1) (x `div` 2)

-- The precondition is pretty sparse, we use custom generators when running this property
toMatrixPermuteProp :: P.Perm -> Integer -> Property
toMatrixPermuteProp perm i =
  0 <= i ==> i < 2^n ==>  
    fromBits (P.permute perm (toBits n i)) == B.transformInt (B.fromPerm perm) i
  where n = P.size perm
  