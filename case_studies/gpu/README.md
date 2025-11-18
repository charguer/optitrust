# GPU/CUDA case studies

These case studies are GPU kernels written in OptiTrust, using language extensions (currently under development) for representing GPU programs. Unless otherwise noted these algorithms all come from the official [cuda samples repository](https://github.com/NVIDIA/cuda-samples/tree/c94ff366aed18c797b8a85dfaac7817b0228b420). Several of them have an `_opt.cu` file in the case studies directory explaining in plain english how the optimized kernel is obtained, as a supplement.

Description/status of each:
* [`vector_add`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/0_Introduction/vectorAdd/vectorAdd.cu): add a constant to a vector
  - [x] Full functional correctness
  - [x] Optimized GPU version
  - [ ] CPU->GPU Transformation script
* [`reduce`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/2_Concepts_and_Techniques/reduction/reduction_kernel.cu): reduce/sum a vector
  - [x] Full functional correctness
  - [ ] Unoptimized GPU version
  - [ ] Optimized GPU version
  - [ ] CPU->GPU Transformation script
* [`transpose`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/6_Performance/transpose/transpose.cu): transposition of a matrix
  - [ ] Full functional correctness
  - [ ] Unoptimized GPU version
  - [ ] Optimized GPU version
  - [ ] CPU->GPU Transformation script
* [`histogram`](https://github.com/NVIDIA/cuda-samples/blob/c94ff366aed18c797b8a85dfaac7817b0228b420/Samples/2_Concepts_and_Techniques/histogram/histogram256.cu): simple histogram computation
  - [ ] Full functional correctness
  - [ ] Unoptimized GPU version
  - [ ] Optimized GPU version
  - [ ] CPU->GPU Transformation script
