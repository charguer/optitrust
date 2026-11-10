// kernels.cuh — reduction kernels under study.
//
// Each kernel reduces N elements into N/512 partial sums (one per block).
// The host then sums the partial sums to get the final scalar.
//
// Kernel signature (all three kernels):
//   __global__ void kernel(float* partial_sums, const float* arr, int N)
//   launched as: kernel<<<N/512, 256, 256*sizeof(float)>>>(d_partial, d_arr, N)
//
// To add a new kernel:
//   1. Write your __global__ function below.
//   2. Register it in allKernels() in benchmark.cuh.
#pragma once
#include "common.h"

#define REDUCE_TPB 256   // threads per block
#define REDUCE_EPT 2     // elements per thread (512/256)
#define REDUCE_SMEM (REDUCE_TPB * sizeof(float))

// ===========================================================================
// PASTE YOUR KERNELS HERE
// ===========================================================================
//
// --- Kernel 0: OptiGPU generated ---
// __global__ void __kernel0(float* partial_sums, const float* arr, int N) {
//   ... your OptiGPU kernel body ...
// }
//
// --- Kernel 1: Reference (reduce3) ---
// __global__ void reduce3_k(float* partial_sums, const float* arr, int N) {
//   ... your reduce3<float> body ...
// }
//
// --- Kernel 2: Best performing (reduce6) ---
// __global__ void reduce6_k(float* partial_sums, const float* arr, int N) {
//   ... your reduce6<float,256,true> body ...
// }
//
// ===========================================================================
// PLACEHOLDERS — replace with your actual kernels.
// These are s²e tree-reduction stubs so the build succeeds; they exist
// only so you can verify the harness works before pasting real code.
// ===========================================================================


#include <cooperative_groups.h>
namespace cg = cooperative_groups;

// Utility class used to avoid linker errors with extern
// unsized shared memory arrays with templated type
template <class T> struct SharedMemoryUtil
{
    __device__ inline operator T *()
    {
        extern __shared__ int __smem[];
        return (T *)__smem;
    }

    __device__ inline operator const T *() const
    {
        extern __shared__ int __smem[];
        return (T *)__smem;
    }
};

#include <optitrust_gpu_cuda.cuh>

template <class T> __global__ void reduce0(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // load shared mem
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * blockDim.x + threadIdx.x;

    sdata[tid] = (i < n) ? g_idata[i] : 0;

    cg::sync(cta);

    // do reduction in shared mem
    for (unsigned int s = 1; s < blockDim.x; s *= 2) {
        // modulo arithmetic is slow!
        if ((tid % (2 * s)) == 0) {
            sdata[tid] += sdata[tid + s];
        }

        cg::sync(cta);
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = sdata[0];
}

template <class T> __global__ void reduce1(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // load shared mem
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * blockDim.x + threadIdx.x;

    sdata[tid] = (i < n) ? g_idata[i] : 0;

    cg::sync(cta);

    // do reduction in shared mem
    for (unsigned int s = 1; s < blockDim.x; s *= 2) {
        int index = 2 * s * tid;

        if (index < blockDim.x) {
            sdata[index] += sdata[index + s];
        }

        cg::sync(cta);
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = sdata[0];
}

template <class T> __global__ void reduce2(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // load shared mem
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * blockDim.x + threadIdx.x;

    sdata[tid] = (i < n) ? g_idata[i] : 0;

    cg::sync(cta);

    // do reduction in shared mem
    for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s) {
            sdata[tid] += sdata[tid + s];
        }

        cg::sync(cta);
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = sdata[0];
}

/*
    This version uses n/2 threads --
    it performs the first level of reduction when reading from global memory.
*/
template <class T> __global__ void reduce3(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * (blockDim.x * 2) + threadIdx.x;

    T mySum = (i < n) ? g_idata[i] : 0;

    if (i + blockDim.x < n)
        mySum += g_idata[i + blockDim.x];

    sdata[tid] = mySum;
    cg::sync(cta);

    // do reduction in shared mem
    for (unsigned int s = blockDim.x / 2; s > 0; s >>= 1) {
        if (tid < s) {
            sdata[tid] = mySum = mySum + sdata[tid + s];
        }

        cg::sync(cta);
    }

    // write result for this block to global mem
    if (tid == 0)
        g_odata[blockIdx.x] = mySum;
}

template <class T, unsigned int blockSize> __global__ void reduce4(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * (blockDim.x * 2) + threadIdx.x;

    T mySum = (i < n) ? g_idata[i] : 0;

    if (i + blockSize < n)
        mySum += g_idata[i + blockSize];

    sdata[tid] = mySum;
    cg::sync(cta);

    // do reduction in shared mem
    for (unsigned int s = blockDim.x / 2; s > 32; s >>= 1) {
        if (tid < s) {
            sdata[tid] = mySum = mySum + sdata[tid + s];
        }

        cg::sync(cta);
    }

    cg::thread_block_tile<32> tile32 = cg::tiled_partition<32>(cta);

    if (cta.thread_rank() < 32) {
        // Fetch final intermediate sum from 2nd warp
        if (blockSize >= 64)
            mySum += sdata[tid + 32];
        // Reduce final warp using shuffle
        for (int offset = tile32.size() / 2; offset > 0; offset /= 2) {
            mySum += tile32.shfl_down(mySum, offset);
        }
    }

    // write result for this block to global mem
    if (cta.thread_rank() == 0)
        g_odata[blockIdx.x] = mySum;
}

template <class T, unsigned int blockSize> __global__ void reduce5(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i   = blockIdx.x * (blockSize * 2) + threadIdx.x;

    T mySum = (i < n) ? g_idata[i] : 0;

    if (i + blockSize < n)
        mySum += g_idata[i + blockSize];

    sdata[tid] = mySum;
    cg::sync(cta);

    // do reduction in shared mem
    if ((blockSize >= 512) && (tid < 256)) {
        sdata[tid] = mySum = mySum + sdata[tid + 256];
    }

    cg::sync(cta);

    if ((blockSize >= 256) && (tid < 128)) {
        sdata[tid] = mySum = mySum + sdata[tid + 128];
    }

    cg::sync(cta);

    if ((blockSize >= 128) && (tid < 64)) {
        sdata[tid] = mySum = mySum + sdata[tid + 64];
    }

    cg::sync(cta);

    cg::thread_block_tile<32> tile32 = cg::tiled_partition<32>(cta);

    if (cta.thread_rank() < 32) {
        // Fetch final intermediate sum from 2nd warp
        if (blockSize >= 64)
            mySum += sdata[tid + 32];
        // Reduce final warp using shuffle
        for (int offset = tile32.size() / 2; offset > 0; offset /= 2) {
            mySum += tile32.shfl_down(mySum, offset);
        }
    }

    // write result for this block to global mem
    if (cta.thread_rank() == 0)
        g_odata[blockIdx.x] = mySum;
}

template <class T, unsigned int blockSize, bool nIsPow2> __global__ void reduce6(T *g_odata, const T *g_idata, int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemoryUtil<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid      = threadIdx.x;
    unsigned int gridSize = blockSize * gridDim.x;

    T mySum = 0;

    // we reduce multiple elements per thread.  The number is determined by the
    // number of active thread blocks (via gridDim).  More blocks will result
    // in a larger gridSize and therefore fewer elements per thread
    if (nIsPow2) {
        unsigned int i = blockIdx.x * blockSize * 2 + threadIdx.x;
        gridSize       = gridSize << 1;

        while (i < n) {
            mySum += g_idata[i];
            // ensure we don't read out of bounds -- this is optimized away for
            // powerOf2 sized arrays
            if ((i + blockSize) < n) {
                mySum += g_idata[i + blockSize];
            }
            i += gridSize;
        }
    }
    else {
        unsigned int i = blockIdx.x * blockSize + threadIdx.x;
        while (i < n) {
            mySum += g_idata[i];
            i += gridSize;
        }
    }

    // each thread puts its local sum into shared memory
    sdata[tid] = mySum;
    cg::sync(cta);

    // do reduction in shared mem
    if ((blockSize >= 512) && (tid < 256)) {
        sdata[tid] = mySum = mySum + sdata[tid + 256];
    }

    cg::sync(cta);

    if ((blockSize >= 256) && (tid < 128)) {
        sdata[tid] = mySum = mySum + sdata[tid + 128];
    }

    cg::sync(cta);

    if ((blockSize >= 128) && (tid < 64)) {
        sdata[tid] = mySum = mySum + sdata[tid + 64];
    }

    cg::sync(cta);

    cg::thread_block_tile<32> tile32 = cg::tiled_partition<32>(cta);

    if (cta.thread_rank() < 32) {
        // Fetch final intermediate sum from 2nd warp
        if (blockSize >= 64)
            mySum += sdata[tid + 32];
        // Reduce final warp using shuffle
        for (int offset = tile32.size() / 2; offset > 0; offset /= 2) {
            mySum += tile32.shfl_down(mySum, offset);
        }
    }

    // write result for this block to global mem
    if (cta.thread_rank() == 0)
        g_odata[blockIdx.x] = mySum;
}

__global__ void reduceOptiGPU(float* d_partial_sums, const float* d_arr, int N)  /*@*/{
  const int __ctx_sz = MSIZE1(exact_div(N, 512)) * MSIZE1(256);
  const int __tid = blockIdx.x * MSIZE1(256) + threadIdx.x;
  SharedMemory smem;
  float* const tile = (float*) smem.ptr(sizeof(float) * MSIZE1(256));
  const int __ctx_sz_0 = __ctx_sz / (exact_div(N, 512));
  const int __bi0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / 256;
  const int __ti1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz_0 / (1 << 8);
  const int __t2 = __tid % __ctx_sz_0 / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_0 / 256;
  const int __ti_f3 = __tid % __ctx_sz_0 / __ctx_sz_3;
  tile[MINDEX2(exact_div(N, 512), 256, 0, __ti1)] = 0.f;
   for (int i = 0; i < 2; i++) {
    tile[MINDEX2(exact_div(N, 512), 256, 0, __ti1)] = tile[MINDEX2(exact_div(N, 512), 256, 0, __ti1)] + d_arr[MINDEX1(N, __bi0 * 512 + (
      __ti1 * 2 + i))];
  }
  __syncthreads();
  float* const reduce_arr_7 = &tile[MINDEX2(exact_div(N, 512), 256, 0, 0)];
   for (int t = 0; t < 1 << 8; t++) {  }
   for (int i = 8; i > 0; i--) {
    const int ei = 1 << i - 1;
    const int eii = 1 << i;
    if (__t2 < 1 << i - 1) {
      reduce_arr_7[MINDEX1(1 << 8, __t2)] = reduce_arr_7[MINDEX1(1 << 8, __t2)] + (
        &reduce_arr_7[ei])[MINDEX1(ei, __t2)];
    }
    else {  }
    __syncthreads();
  }
  if (__ti_f3 == 0) {
    const float sum_temp_8 = reduce_arr_7[MINDEX1(1 << 8, 0)];
     for (int t = 0; t < 1 << 8; t++) {  }
    d_partial_sums[MINDEX1(exact_div(N, 512), __bi0)] = sum_temp_8;
  }
  else {  }
}/*@*/

__global__ void reduceOptiGPUSimplified(float* __restrict__ d_partial_sums,
                                        const float*       __restrict__ d_arr,
                                        int N) {
    __shared__ float tile[256];

    const int bid = blockIdx.x;
    const int tid = threadIdx.x;

    // Each thread accumulates 2 consecutive elements.
    tile[tid] = d_arr[bid * 512 + tid * 2]
              + d_arr[bid * 512 + tid * 2 + 1];
    __syncthreads();

    // Standard tree reduction: 8 levels (2^8 = 256 threads).
    #pragma unroll
    for (int s = 128; s > 0; s >>= 1) {
        if (tid < s)
            tile[tid] += tile[tid + s];
        __syncthreads();
    }

    // Thread 0 of each block writes the partial sum.
    if (tid == 0)
        d_partial_sums[bid] = tile[0];
}

__global__ void reduceOptiGPUMySum(float* d_partial_sums, const float* d_arr, int N)  /*@*/{
    __shared__ float tile[256];

    const int bid = blockIdx.x;
    const int tid = threadIdx.x;

    // Each thread accumulates 2 consecutivemySum elements.
    float mySum = d_arr[bid * 512 + tid * 2] + d_arr[bid * 512 + tid * 2 + 1];
    tile[tid] = mySum;
    __syncthreads();

    // Standard tree reduction: 8 levels (2^8 = 256 threads).
    #pragma unroll
    for (int s = 128; s > 0; s >>= 1) {
        if (tid < s) {
          mySum += tile[tid + s];
          tile[tid] = mySum;
        }
        __syncthreads();
    }

    // Thread 0 of each block writes the partial sum.
    if (tid == 0)
        d_partial_sums[bid] = mySum;
}/*@*/

__global__ void reduceQwen(float* __restrict__ partial_sums,
                            const float* __restrict__ arr, int N) {
  __shared__ float sdata[REDUCE_TPB];
  int tid = threadIdx.x;
  int gid = blockIdx.x * REDUCE_TPB * REDUCE_EPT + threadIdx.x * REDUCE_EPT;
  float val = 0.f;
  #pragma unroll
  for (int i = 0; i < REDUCE_EPT; ++i)
    val += arr[gid + i];
  sdata[tid] = val;
  __syncthreads();
  for (int s = REDUCE_TPB / 2; s > 0; s >>= 1) {
    if (tid < s) sdata[tid] += sdata[tid + s];
    __syncthreads();
  }
  if (tid == 0) partial_sums[blockIdx.x] = sdata[0];
}
