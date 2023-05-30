module Main ( main ) where

import Test.Tasty
import PermTests ( permTests )
import BmmcTests ( bmmcTests )
import ParmMatrixTests ( parmMatrixTests )


main :: IO ()
main = defaultMain $ testGroup "All Tests" 
  [ permTests
  , parmMatrixTests
  , bmmcTests
  ]

