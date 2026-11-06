
// kernels.cuh — all transpose implementations under study.
//
//   A : input,  H rows x W cols, row-major  (index = y*W + x)
//   B : output, W rows x H cols, row-major  (index = r*H + c), B[r][c] = A[c][r]
//
// To add a new kernel:
//   1. Write your __global__ function below with signature:
//        __global__ void myKernel(const float* A, float* B, int H, int W)
//   2. Register it in the allKernels() vector in benchmark.cu.
#pragma once
#include "common.h"

#define TILE_DIM 32

//// KERNELS

#include <optitrust_gpu_cuda.cuh>
#include <cooperative_groups.h>

namespace cg = cooperative_groups;

#define BLOCK_ROWS 16

__global__ void transposeNoBankConflicts(const float *idata, float *odata, int height, int width)
{
    // Handle to thread block group
    cg::thread_block cta = cg::this_thread_block();
    __shared__ float tile[TILE_DIM][TILE_DIM+1];

    int xIndex   = blockIdx.x * TILE_DIM + threadIdx.x;
    int yIndex   = blockIdx.y * TILE_DIM + threadIdx.y;
    int index_in = xIndex + (yIndex)*width;

    xIndex        = blockIdx.y * TILE_DIM + threadIdx.x;
    yIndex        = blockIdx.x * TILE_DIM + threadIdx.y;
    int index_out = xIndex + (yIndex)*height;

    for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
        tile[threadIdx.y + i][threadIdx.x] = idata[index_in + i * width];
    }

    cg::sync(cta);

    for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
        odata[index_out + i * height] = tile[threadIdx.x][threadIdx.y + i];
    }
}

 __global__ void transposeOptiTrust (const float* d_a, float* d_b, int H, int W)  /*@*/{
  const int __ctx_sz = MSIZE2(exact_div(H, 32), exact_div(W, 32)) * MSIZE2(16, 32);
  const int __tid = blockIdx.x * MSIZE2(16, 32) + threadIdx.x;
  SharedMemory smem;
  float* const tile = (float*) smem.ptr(sizeof(float) * MSIZE2(32, 32));
  const int __ctx_sz_0 = __ctx_sz / (exact_div(H, 32));
  const int __by0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / (exact_div(W, 32));
  const int __bx1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz_1 / 16;
  const int __y2 = __tid % __ctx_sz_1 / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_2 / 32;
  const int __x3 = __tid % __ctx_sz_2 / __ctx_sz_3;
  const int __ctx_sz_4 = __ctx_sz_1 / 16;
  const int __x4 = __tid % __ctx_sz_1 / __ctx_sz_4;
  const int __ctx_sz_5 = __ctx_sz_4 / 32;
  const int __y5 = __tid % __ctx_sz_4 / __ctx_sz_5;
   for (int j = 0; j < 2; j++) {
    tile[MINDEX3(exact_div(H, 32) * (exact_div(W, 32)), 32, 32, 0, j * 16 + __y2, __x3)] = d_a[MINDEX2(H, W, __by0 * 32 + (
      j * 16 + __y2), __bx1 * 32 + __x3)];
  }
  __syncthreads();
   for (int j = 0; j < 2; j++) {
    d_b[MINDEX2(W, H, __bx1 * 32 + (j * 16 + __x4), __by0 * 32 + __y5)] = tile[MINDEX3(exact_div(H, 32) * (
      exact_div(W, 32)), 32, 32, 0, __y5, j * 16 + __x4)];
  }
}

 __global__ void transposeOptiTrustSimplified(const float* d_a, float* d_b, int H, int W)  /*@*/{
  // const int dH = H / 32;
  const int dW = W / 32;

  // 1D grid of dH*dW blocks, 512 threads each (16×32 logical).
  const int tid = blockIdx.x * 512 + threadIdx.x;
  // Decompose linear tid into 4 indices:
  const int by =  tid / (dW * 512);        // block row   [0, dH)
  const int bx = (tid % (dW * 512)) / 512; // block col   [0, dW)
  const int y  = (tid % 512) / 32;         // row-in-half [0,16)
  const int x  = tid % 32;                 // col         [0,32)

  SharedMemory smem;
  float* const tile = (float*) smem.ptr(sizeof(float) * 32 * 32);

  for (int j = 0; j < 2; j++) {
    tile[(j * 16 + y) * 32 + x] = d_a[(by * 32 + j * 16 + y) * W + (bx * 32 + x)];
  }
  __syncthreads();
  for (int j = 0; j < 2; j++) {
    d_b[(bx * 32 + j * 16 + y) * H + (by * 32 + x)] = tile[x * 32 + j * 16 + y];
  }
}

//// QWEN KERNELS

// ---------------------------------------------------------------------------
// Naive transpose: direct global read + write.
// ---------------------------------------------------------------------------
__global__ void transposeNaiveQwen(const float* __restrict__ A,
                               float*       __restrict__ B,
                               int H, int W) {
  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  if (x < W && y < H)
    B[x * H + y] = A[y * W + x];
}

// ---------------------------------------------------------------------------
// Bank-conflict-free tiled transpose.
// ---------------------------------------------------------------------------
__global__ void transposeNoBankConflictsQwen(const float* __restrict__ A,
                                         float*       __restrict__ B,
                                         int H, int W) {
  __shared__ float tile[TILE_DIM][TILE_DIM + 1];

  int x = blockIdx.x * TILE_DIM + threadIdx.x;
  int y = blockIdx.y * TILE_DIM + threadIdx.y;
  if (x < W && y < H)
    tile[threadIdx.y][threadIdx.x] = A[y * W + x];
  __syncthreads();

  int r = blockIdx.x * TILE_DIM + threadIdx.y;
  int c = blockIdx.y * TILE_DIM + threadIdx.x;
  if (r < W && c < H)
    B[r * H + c] = tile[threadIdx.x][threadIdx.y];
}

// ---------------------------------------------------------------------------
// EXAMPLE: how to add a third kernel (uncomment and register in benchmark.cu)
// ---------------------------------------------------------------------------
// __global__ void transposeVectorized(const float* __restrict__ A,
//                                     float*       __restrict__ B,
//                                     int H, int W) {
//   // your implementation here
// }

inline dim3 gridFor(int H, int W, int block) {
  return dim3((W + block - 1) / block, (H + block - 1) / block);
}
