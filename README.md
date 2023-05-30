# Summary

This artifact allows you to generate CUDA kernels for different BPC and BMMC permutations. The kernel generation is written in Haskell. For simplicity the version presented here does not handle complement vectors of BPC and BMMC permutations. The implementation of permutations and BMMC matrices we use is in the files src/Perm.hs and src/Bmmc.hs, and tested in the files tests/PermTests.hs and tests/BmmcTests.hs. The actual kernel generation takes place in the file src/KernelGen.hs. 

The code uses slightly different notation for bit partitions compared to the paper : 
  'n' remains 'n'
  'n_tile' corresponds to 'p'
  'n_tile - n_over' corresponds to 'q'

# Generating the kernels

The Haskell code that generates permutation kernels is in the directories 'exe' and 'src' and has Quickcheck tests in 'tests'. To run the file exe/Main.hs you will need a working cabal installation. Use :
cabal run main

To run the tests use :
cabal test --test-show-details=direct

To cleanup use :
cabal clean

# Benchmarking the kernels

To benchmark the generated kernels, paste them in the file 'benchmark.cu' and modify the main function to benchmark your kernels. You will need a working CUDA installation. To compile and run the benchmarks run :
nvcc -o a.out benchmark.cu && ./a.out


