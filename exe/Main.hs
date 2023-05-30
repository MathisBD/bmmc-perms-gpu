module Main ( main ) where 

import Data.List ( sort, transpose, intersperse, intercalate )
import Data.List.Unique ( count )
import Control.Monad ( forM_, liftM2, replicateM )
import Data.Traversable ( for )
import System.Random ( randomIO, randomRIO )
import qualified Bmmc as B
import qualified Data.Vector.Unboxed as U
import qualified Perm as P
import KernelGen
import qualified Control.Category as P
import Text.Printf ( printf )
import Control.Exception ( assert )
import Data.Bits ( Bits(shiftR) ) 
import qualified Test.Tasty.QuickCheck as QC
import System.IO


-- This example shows how to generate the various kernels for a BPC permutation.
-- Uncomment the lines below to get all kernels.
exampleBPC :: P.Perm -> Handle -> IO ()
exampleBPC perm file = do 
  let n = P.size perm 
      p = 5
      iters = 3
  -- BPC kernels.
  hPutStrLn file $ generate       "BPC_naive_kernel"            perm
  hPutStrLn file $ generateC      "BPC_tile_kernel"             perm p
  --hPutStrLn file $ generateCB     "BPC_tile_banks_kernel"       perm p
  --hPutStrLn file $ generateCI     "BPC_tile_iters_kernel"       perm p iters
  --hPutStrLn file $ generateCBI    "BPC_tile_banks_iters_kernel" perm p iters
  -- Tiled BMMC kernels.
  --hPutStrLn file $ generateBmmc   "BMMC_naive_kernel"           (B.fromPerm perm)
  --hPutStrLn file $ generateBmmcC  "BMMC_tile_kernel"            (B.fromPerm perm) p
  --hPutStrLn file $ generateBmmcCB "BMMC_tile_banks_kernel"      (B.fromPerm perm) p

-- This example shows how to generate the two tiled BMMC kernels
-- corresponding to a BMMC permutation.
exampleBMMC :: B.BMatrix -> Handle -> IO ()
exampleBMMC mat _ | not (B.isInvertible mat) = error "The BMMC matrix must be invertible !"
exampleBMMC mat file = do 
  -- Factorize the BMMC matrix.
  let n = B.rows mat
      p = 5
      (upper, lower, perm) = B.decomposeULP mat
      rev = B.fromPerm (P.reverse n)
      tiled1 = rev `B.mult` lower `B.mult` B.fromPerm perm
      tiled2 = upper `B.mult` rev 
  -- Print the BMMC matrix.
  hPutStrLn file $ "The matrix is :\n" <> show mat <> "\n"
  hPutStrLn file $ "\nIs the decomposition correct ? " <> show (mat == tiled2 `B.mult` tiled1) <> "\n"
  -- Tiled BMMC kernels.
  hPutStrLn file $ generateBmmcC "BMMC_kernel_first" tiled1 p
  hPutStrLn file $ generateBmmcC "BMMC_kernel_second" tiled2 p

-- Uncomment/comment the relevant lines to use the example you want.
main :: IO ()
main = do
  let perm = QC.generate $ P.randPerm 20
  withFile "bpc_kernels.txt" WriteMode $ exampleBPC perm
  --mat <- QC.generate $ B.randInvMatrix 20
  --withFile "bmmc_kernels.txt" WriteMode $ exampleBMMC mat

