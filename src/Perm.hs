module Perm (
  Perm, size, fromList, toList, make,
  identity, Perm.reverse, swap,
  compose, inverse, apply, permute,
  randPerm
) where 

import System.Random ( randomIO, randomRIO )
import qualified Data.Vector.Unboxed as U
import qualified Test.Tasty.QuickCheck as QC


-- The permutation is defined as the mapping between the indices
-- and the corresponding elements.
type Perm = U.Vector Int

-- The number of elements this permutation acts on.
size :: Perm -> Int
size = U.length

-- The image of an index through a permutation.
apply :: Perm -> Int -> Int
apply perm i | i < 0 || i >= size perm = error "out of bounds index when applying permutation"
apply perm i = perm U.! i

-- Make a permutation given a mapping.
make :: Int -> (Int -> Int) -> Perm
make = U.generate

fromList :: [Int] -> Perm
fromList = U.fromList

toList :: Perm -> [Int]
toList = U.toList

-- The identity permutation on n elements.
identity :: Int -> Perm 
identity n = make n id

-- The reverse permutation on n elements.
reverse :: Int -> Perm 
reverse n = make n $ \i -> n - 1 - i

-- The permutation that swaps two indices.
swap :: Int -> Int -> Int -> Perm
swap n k1 k2 = make n $ \i -> if i == k1 then k2 
                              else if i == k2 then k1
                              else i

-- A random permutation on n elements
randPerm :: Int -> QC.Gen Perm
randPerm n = fromList <$> QC.shuffle [0..n-1] 

instance QC.Arbitrary Perm where
  arbitrary = QC.sized randPerm

-- Compose two permutations :
--   compose p2 p1 == p2 . p1 
compose :: Perm -> Perm -> Perm
compose p2 p1
  | size p1 /= size p2 = error "Perm.compose : can only compose permutations of the same size"
  | otherwise = make n $ \i -> apply p2 $ apply p1 i
  where n = size p1

-- Compute the inverse of a permutation.
inverse :: Perm -> Perm 
inverse perm = make n $ \i -> indexOf i (U.toList perm)
  where n = size perm
        indexOf i [] = undefined
        indexOf i (x:xs) 
          | i == x    = 0
          | otherwise = 1 + indexOf i xs

-- Apply a permutation to a list of arbitrary elements.
permute :: Perm -> [a] -> [a]
permute perm xs = [ xs !! apply inv i | i <- [0..n-1] ]
  where n = size perm
        inv = inverse perm

