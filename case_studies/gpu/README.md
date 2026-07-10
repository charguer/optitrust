# GPU/CUDA case studies

These case studies are GPU kernels written in OptiTrust, using language extensions for representing GPU programs (OptiGPU).

Several case studies have an `_opt.cu` file in the case studies directory explaining in plain english how the optimized kernel is obtained, as a supplement. The "_gpu.cpp" file in each directory is a handwritten implementation of the GPU program in OptiTrust, i.e. generating GPU code from OptiTrust without making use of the transformations.

## Kernels from the official [CUDA samples repository](https://github.com/NVIDIA/cuda-samples/tree/c94ff366aed18c797b8a85dfaac7817b0228b420)

* [`vector_add`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/0_Introduction/vectorAdd/vectorAdd.cu): add a constant to a vector
  - [x] Full functional correctness
  - [x] Optimized GPU version
  - [X] CPU->GPU Transformation script
* [`reduce`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/2_Concepts_and_Techniques/reduction/reduction_kernel.cu): reduce/sum a vector
  - [x] Full functional correctness
  - [x] Unoptimized GPU version
  - [x] Optimized GPU version
  - [x] CPU->GPU Transformation script
* [`transpose`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/6_Performance/transpose/transpose.cu): transposition of a matrix
  - [X] Full functional correctness
  - [X] Unoptimized GPU version
  - [X] Optimized GPU version
  - [x] CPU->GPU Transformation script
* [`histogram`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/2_Concepts_and_Techniques/histogram/histogram256.cu): simple histogram computation
  - [X] Full functional correctness
  - [ ] Unoptimized GPU version
  - [ ] Optimized GPU version
  - [ ] CPU->GPU Transformation script

## Kernels from blogs

* [`matmul`](https://github.com/siboehm/SGEMM_CUDA/tree/master/src/kernels): optimized matmul (adapted from [Simon Boehm's blog](https://siboehm.com/articles/22/CUDA-MMM), not from CUDA samples) with coalescing and hierarchical tiling for shared memory and thread registers storage
  - [X] Full functional correctness
  - [X] Optimized GPU version
  - [x] CPU->GPU Transformation script
  - [ ] Explicit vectorization, tensor cores, async double buffering
