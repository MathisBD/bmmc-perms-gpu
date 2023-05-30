module BmmcTests ( bmmcTests ) where 

import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Perm as P
import qualified Bmmc as B
import Data.List ( sort, isPrefixOf, find, isSubsequenceOf )
import Control.Monad ( liftM2 )
import Bmmc (decomposeULP)
import Data.Char (isUpper)


bmmcTests :: TestTree
bmmcTests = testGroup "BMMC Tests" 
  [ testProperty "empty-size" emptySizeProp
  , testProperty "id-size" idSizeProp
  , testProperty "zeros-size" zerosSizeProp
  , testProperty "ones-size" onesSizeProp
  , testProperty "add-size" addSizeProp
  , testProperty "mult-size" multSizeProp
  , testProperty "transpose-size" transposeSizeProp
  , testProperty "echelon-size" echelonSizeProp
  , testProperty "inv-size" invSizeProp
  , testProperty "hstack-size" hstackSizeProp
  , testProperty "vstack-size" vstackSizeProp
  , testProperty "hsplit-size" hsplitSizeProp
  , testProperty "vsplit-size" vsplitSizeProp
  , testProperty "row-from-to-int" rowFromToInt
  , testProperty "row-to-from-int" $
      forAllShrink (sized $ \n -> B.randMatrix 1 n) shrink rowToFromInt
  , testProperty "col-from-to-int" colFromToInt
  , testProperty "col-to-from-int" $
      forAllShrink (sized $ \n -> B.randMatrix n 1) shrink colToFromInt
  , testProperty "inv" invProp
  , testProperty "inv-inv" invInvProp
  , testProperty "inv-mult" invMultProp
  , testProperty "rank-bounds" rankBoundsProp
  , testProperty "rank-inv" rankInvProp
  , testProperty "transform-range" transformRangeProp
  , testProperty "from-perm-mult" fromPermMultProp
  , testProperty "from-perm-id" fromPermIdProp
  , testProperty "from-perm-inv" fromPermInvProp
  , testProperty "inv-from-perm" invFromPermProp
  , testProperty "decomposeULP" decomposeULPProp
  , testProperty "decomposeLUP" decomposeLUPProp
  ]

emptySizeProp :: Property
emptySizeProp = B.rows B.empty == 0 .&&. B.cols B.empty == 0

idSizeProp :: Int -> Property 
idSizeProp n = 0 <= n ==> B.rows (B.identity n) == n && B.cols (B.identity n) == n

zerosSizeProp :: Int -> Int -> Property 
zerosSizeProp r c = 
  0 <= r ==> 0 <= c ==> 
    B.rows (B.zeros r c) == r && B.cols (B.zeros r c) == c

onesSizeProp :: Int -> Int -> Property 
onesSizeProp r c = 
  0 <= r ==> 0 <= c ==> 
    B.rows (B.ones r c) == r && B.cols (B.ones r c) == c

addSizeProp :: B.BMatrix -> B.BMatrix -> Property
addSizeProp a b =
  B.rows a == B.rows b ==> B.cols a == B.cols b ==>
    B.rows c == B.rows a && B.cols c == B.cols a
  where c = a `B.add` b

multSizeProp :: B.BMatrix -> B.BMatrix -> Property
multSizeProp a b =
  B.cols a == B.rows b ==>
    B.rows c == B.rows a && B.cols c == B.cols b
  where c = a `B.mult` b

transposeSizeProp :: B.BMatrix -> Property
transposeSizeProp a = B.rows (B.transpose a) == B.cols a .&&. B.cols (B.transpose a) == B.rows a

echelonSizeProp :: B.BMatrix -> Property
echelonSizeProp a = B.rows b == B.rows a .&&. B.cols b == B.cols a
  where b = B.reducedRowEchelon a

invSizeProp :: B.BMatrix -> Property
invSizeProp a = 
  B.isInvertible a ==>
    B.rows b == B.rows a .&&. B.cols b == B.cols a
  where b = B.unsafeInverse a

hstackSizeProp :: B.BMatrix -> B.BMatrix -> Property
hstackSizeProp a b = 
  B.rows a == B.rows b ==> 
    B.rows c == B.rows a && B.cols c == B.cols a + B.cols b
  where c = B.hstack a b

vstackSizeProp :: B.BMatrix -> B.BMatrix -> Property
vstackSizeProp a b = 
  B.cols a == B.cols b ==> 
    B.cols c == B.cols a && B.rows c == B.rows a + B.rows b
  where c = B.vstack a b
  
hsplitSizeProp :: B.BMatrix -> Int -> Property
hsplitSizeProp c k =
  0 <= k ==> k <= B.cols c ==>
    B.rows a == B.rows c && B.rows b == B.rows c && B.cols a == k && B.cols b == B.cols c - k
  where (a, b) = B.hsplit k c

vsplitSizeProp :: B.BMatrix -> Int -> Property
vsplitSizeProp c k =
  0 <= k ==> k <= B.rows c ==>
    B.cols a == B.cols c && B.cols b == B.cols c && B.rows a == k && B.rows b == B.rows c - k
  where (a, b) = B.vsplit k c

rowToFromInt :: B.BMatrix -> Property
rowToFromInt r = 
  B.rows r == 1 ==>
    B.rowFromInt (B.cols r) (B.rowToInt r) === r

rowFromToInt :: Integer -> Int -> Property
rowFromToInt x n = 
  0 < n ==> 0 <= x ==> x < 2^n ==>
    B.rowToInt (B.rowFromInt n x) === x

colToFromInt :: B.BMatrix -> Property
colToFromInt c = 
  B.cols c == 1 ==>
    B.colFromInt (B.rows c) (B.colToInt c) === c

colFromToInt :: Integer -> Int -> Property
colFromToInt x n = 
  0 < n ==> 0 <= x ==> x < 2^n ==>
    B.colToInt (B.colFromInt n x) === x

isSorted :: Ord a => [a] -> Bool
isSorted xs = xs == sort xs

fromPermIdProp :: Int -> Property
fromPermIdProp n = n >= 0 ==> B.fromPerm (P.identity n) == B.identity n

fromPermMultProp :: P.Perm -> P.Perm -> Property
fromPermMultProp p1 p2 = 
  P.size p1 == P.size p2 ==>
    B.fromPerm (p1 `P.compose` p2) == B.fromPerm p1 `B.mult` B.fromPerm p2  
  where n = P.size p1

fromPermInvProp :: P.Perm -> Property
fromPermInvProp perm = property $ B.isInvertible (B.fromPerm perm)

invFromPermProp :: P.Perm -> Property
invFromPermProp perm = B.inverse (B.fromPerm perm) === Just (B.fromPerm (P.inverse perm))

invProp :: B.BMatrix -> Property
invProp a = 
  B.isInvertible a ==> 
    b `B.mult` a == B.identity n && a `B.mult` b == B.identity n
  where n = B.rows a
        b = B.unsafeInverse a

invInvProp :: B.BMatrix -> Property
invInvProp a = 
  B.isInvertible a ==> 
    (B.inverse <$> B.inverse a) == Just (Just a)

invMultProp :: B.BMatrix -> B.BMatrix -> Property
invMultProp a b =
  B.isInvertible a ==> B.isInvertible b ==> B.cols a == B.rows b ==>
    B.inverse (a `B.mult` b) == liftM2 B.mult (B.inverse b) (B.inverse a)

rankBoundsProp :: B.BMatrix -> Property
rankBoundsProp a = 0 <= B.rank a .&&. B.rank a <= B.rows a && B.rank a <= B.cols a

rankInvProp :: B.BMatrix -> Property
rankInvProp a = B.isInvertible a === (B.rank a == B.rows a && B.isSquare a)

transformRangeProp :: B.BMatrix -> Integer -> Property
transformRangeProp a i =
  B.isSquare a ==> 0 <= i ==> i < 2^n ==> 
    0 <= B.transformInt a i && B.transformInt a i < 2^n
  where n = B.rows a

isUpperTriangular :: B.BMatrix -> Bool
isUpperTriangular a = 
  all (\i -> 
    all (\j -> i <= j || not (B.get a i j)) 
      [0..B.cols a - 1]) 
    [0..B.rows a - 1]

isLowerTriangular :: B.BMatrix -> Bool
isLowerTriangular a = 
  all (\i -> 
    all (\j -> i >= j || not (B.get a i j)) 
      [0..B.cols a - 1]) 
    [0..B.rows a - 1]

decomposeULPProp :: B.BMatrix -> Property
decomposeULPProp a = 
  B.isInvertible a ==> 
    isUpperTriangular u && isLowerTriangular l && a == u `B.mult` l `B.mult` B.fromPerm p
  where (u, l, p) = B.decomposeULP a

decomposeLUPProp :: B.BMatrix -> Property
decomposeLUPProp a = 
  B.isInvertible a ==> 
    isUpperTriangular u && isLowerTriangular l && a == l `B.mult` u `B.mult` B.fromPerm p
  where (l, u, p) = B.decomposeLUP a

