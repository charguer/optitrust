// kernels.cuh — matrix-multiplication kernels under study.
//
// All kernels compute: C(m×n) = A(m×p) · B(p×n), all row-major, float.
//
// Signature (uniform across all kernels):
//   __global__ void kernel(float* C, const float* A, const float* B, int m, int n, int p)
//
// To add a new kernel:
//   1. Write your __global__ function below.
//   2. Register it in allKernels() in benchmark.cu.
#pragma once
#include "common.h"

#include <optitrust_gpu_cuda.cuh>

__global__ void mmOptiTrust(float* c_gmem,
                            const float* a_gmem,
                            const float* b_gmem,
                            int m, int n, int p) {
  const int __ctx_sz = MSIZE2(exact_div(m, 32), exact_div(n, 32)) *
                       MSIZE2(exact_div(32, 8), exact_div(32, 4));
  const int __tid =
      blockIdx.x * MSIZE2(exact_div(32, 8), exact_div(32, 4)) + threadIdx.x;
  SharedMemory smem;
  float* const b_smem = (float*)smem.ptr(sizeof(float) * MSIZE3(8, 4, 4));
  float* const a_smem = (float*)smem.ptr(sizeof(float) * MSIZE3(4, 4, 8));
  const int __ctx_sz_0 = __ctx_sz / (exact_div(m, 32));
  const int __bi0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / (exact_div(n, 32));
  const int __bj1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz_1 / 4;
  const int __ti2 = __tid % __ctx_sz_1 / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_2 / 8;
  const int __tj3 = __tid % __ctx_sz_2 / __ctx_sz_3;
  const int __ctx_sz_4 = __ctx_sz_1 / 4;
  const int __ti4 = __tid % __ctx_sz_1 / __ctx_sz_4;
  const int __ctx_sz_5 = __ctx_sz_4 / 8;
  const int __i5 = __tid % __ctx_sz_4 / __ctx_sz_5;
  const int __ctx_sz_6 = __ctx_sz_1 / 8;
  const int __tj6 = __tid % __ctx_sz_1 / __ctx_sz_6;
  const int __ctx_sz_7 = __ctx_sz_6 / 4;
  const int __k7 = __tid % __ctx_sz_6 / __ctx_sz_7;
  const int __ctx_sz_8 = __ctx_sz_1 / 4;
  const int __ti8 = __tid % __ctx_sz_1 / __ctx_sz_8;
  const int __ctx_sz_9 = __ctx_sz_8 / 8;
  const int __tj9 = __tid % __ctx_sz_8 / __ctx_sz_9;
  const int __ctx_sz_10 = __ctx_sz_1 / 4;
  const int __ti10 = __tid % __ctx_sz_1 / __ctx_sz_10;
  const int __ctx_sz_11 = __ctx_sz_10 / 8;
  const int __tj11 = __tid % __ctx_sz_10 / __ctx_sz_11;
  float sum[8 * 4];
   for (int i = 0; i < 8; i++) {
     for (int j = 0; j < 4; j++) {
      sum[MINDEX3(4 * 8, 8, 4, 0, i, j)] = 0.f;
    }
  }
   for (int bkIdx = 0; bkIdx < exact_div(p, 4); bkIdx++) {
     for (int k = 0; k < 4; k++) {
      a_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8, 0, __ti4,
                     k, __i5)] =
          a_gmem[MINDEX2(m, p, __bi0 * 32 + (__ti4 * 8 + __i5), bkIdx * 4 + k)];
    }
     for (int j = 0; j < 4; j++) {
      b_smem[MINDEX4(exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4, 0, __tj6,
                     __k7, j)] =
          b_gmem[MINDEX2(p, n, bkIdx * 4 + __k7, __bj1 * 32 + (__tj6 * 4 + j))];
    }
    __syncthreads();
     for (int k = 0; k < 4; k++) {
      float a_regs[8];
       for (int i = 0; i < 8; i++) {
        a_regs[MINDEX1(8, i)] = a_smem[MINDEX4(
            exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8, 0, __ti8, k, i)];
      }
      float b_regs[4];
       for (int j = 0; j < 4; j++) {
        b_regs[MINDEX1(4, j)] = b_smem[MINDEX4(
            exact_div(m, 32) * (exact_div(n, 32)), 8, 4, 4, 0, __tj9, k, j)];
      }
       for (int i = 0; i < 8; i++) {
         for (int j = 0; j < 4; j++) {
          sum[MINDEX3(4 * 8, 8, 4, 0, i, j)] =
              sum[MINDEX3(4 * 8, 8, 4, 0, i, j)] +
              a_regs[MINDEX1(8, i)] * b_regs[MINDEX1(4, j)];
        }
      }
    }
    __syncthreads();
  }
   for (int i = 0; i < 8; i++) {
     for (int j = 0; j < 4; j++) {
      c_gmem[MINDEX2(m, n, __bi0 * 32 + (__ti10 * 8 + i),
                     __bj1 * 32 + (__tj11 * 4 + j))] =
          sum[MINDEX3(4 * 8, 8, 4, 0, i, j)];
    }
  }
}

// ===========================================================================
// 1. OptiTrust (simplified) — 32×32 tile, 32 threads, k-slice 4
//    Each thread computes 8×4 outputs.
// ===========================================================================
__global__ void mmOptiTrustSimplified(float*       __restrict__ c,
                                      const float* __restrict__ a,
                                      const float* __restrict__ b,
                                      int m, int n, int p) {
    // Block: 32 threads (4×8 logical). Grid: 1D, (m/32)*(n/32).
    // Each block computes a 32×32 tile of C = A(m×p) · B(p×n).
    // Each thread accumulates an 8×4 sub-tile in registers.
    // K-slice per iteration: 4.

    __shared__ float a_smem[4][4][8];  // [row_grp][k][col_in_grp]
    __shared__ float b_smem[8][4][4];  // [col_grp][k][col_in_grp]

    const int dM = m / 32;
    const int dN = n / 32;

    const int bi = blockIdx.x / dN;         // block row  [0, dM)
    const int bj = blockIdx.x % dN;         // block col  [0, dN)
    const int tid = threadIdx.x;

    const int ti  = (tid % 32) / 8;        // thread row-group [0, 4)
    const int tj  = tid % 8;               // thread col       [0, 8)
    const int tjB = (tid % 32) / 4;        // B-load row-group [0, 8)
    const int kB  = tid % 4;               // B-load k         [0, 4)

    float sum[8][4] = {};

    for (int bkIdx = 0; bkIdx < p / 4; ++bkIdx) {
        // Load A tile (coalesced: consecutive tj → consecutive columns).
        #pragma unroll
        for (int k = 0; k < 4; ++k)
            a_smem[ti][k][tj] = a[(bi * 32 + ti * 8 + tj) * p + bkIdx * 4 + k];

        // Load B tile.
        #pragma unroll
        for (int j = 0; j < 4; ++j)
            b_smem[tjB][kB][j] = b[(bkIdx * 4 + kB) * n + bj * 32 + tjB * 4 + j];

        __syncthreads();

        // Accumulate: 4 k-steps × (8×4 outer product).
        #pragma unroll
        for (int k = 0; k < 4; ++k) {
            float a_regs[8];
            float b_regs[4];
            #pragma unroll
            for (int i = 0; i < 8; ++i) a_regs[i] = a_smem[ti][k][i];
            #pragma unroll
            for (int j = 0; j < 4; ++j) b_regs[j] = b_smem[tj][k][j];
            #pragma unroll
            for (int i = 0; i < 8; ++i)
                #pragma unroll
                for (int j = 0; j < 4; ++j)
                    sum[i][j] += a_regs[i] * b_regs[j];
        }

        __syncthreads();
    }

    // Store 8×4 sub-tile.
    #pragma unroll
    for (int i = 0; i < 8; ++i)
        #pragma unroll
        for (int j = 0; j < 4; ++j)
            c[(bi * 32 + ti * 8 + i) * n + bj * 32 + tj * 4 + j] = sum[i][j];
}


// ===========================================================================
// 2. Direct (naive) — no shared memory, each thread one output element.
//    From: CUDA "Matrix Multiply" sample, direct path.
//    Grid: (m*n)/256, Block: 256.
// ===========================================================================
__global__ void mmDirect(float*       __restrict__ c,
                         const float* __restrict__ a,
                         const float* __restrict__ b,
                         int m, int n, int p) {
    int col = threadIdx.x + blockIdx.x * blockDim.x;  // [0, m*n)
    int row = col / n;                                 // [0, m)
    int c_  = col % n;                                 // [0, n)
    float sum = 0.f;
    for (int k = 0; k < p; ++k)
        sum += a[row * p + k] * b[k * n + c_];
    c[col] = sum;
}

// ===========================================================================
// 3. Tiled 16×16 — classic CUDA sample "Tiled Matrix Multiply".
//    Grid: (m/16, n/16), Block: 16×16. Each thread computes 1 output.
//    k-slice: 16.
// ===========================================================================
#define TILE16 16
__global__ void mmTiled16(float*       __restrict__ c,
                           const float* __restrict__ a,
                           const float* __restrict__ b,
                           int m, int n, int p) {
    __shared__ float As[TILE16][TILE16];
    __shared__ float Bs[TILE16][TILE16];

    int row = blockIdx.y * TILE16 + threadIdx.y;
    int col = blockIdx.x * TILE16 + threadIdx.x;
    float sum = 0.f;

    for (int t = 0; t <= (p - 1); t += TILE16) {
        int aCol = t + threadIdx.x;
        int bRow = t + threadIdx.y;
        As[threadIdx.y][threadIdx.x] = (aCol < p) ? a[row * p + aCol] : 0.f;
        Bs[threadIdx.y][threadIdx.x] = (bRow < p && col < n) ? b[bRow * n + col] : 0.f;
        __syncthreads();
        #pragma unroll
        for (int k = 0; k < TILE16; ++k)
            sum += As[threadIdx.y][k] * Bs[k][threadIdx.x];
        __syncthreads();
    }
    if (row < m && col < n) c[row * n + col] = sum;
}

// ===========================================================================
// 4. Tiled 32×32, 2D block — from CUDA "Matrix Multiply" sample (advanced).
//    Grid: (m/32, n/32), Block: 32×32. Each thread computes 1 output.
//    k-slice: 32.  Uses padding to avoid bank conflicts.
// ===========================================================================
#define TILE32 32
__global__ void mmTiled32(float*       __restrict__ c,
                           const float* __restrict__ a,
                           const float* __restrict__ b,
                           int m, int n, int p) {
    __shared__ float As[TILE32][TILE32 + 1];  // +1 padding avoids conflicts
    __shared__ float Bs[TILE32][TILE32 + 1];

    int row = blockIdx.y * TILE32 + threadIdx.y;
    int col = blockIdx.x * TILE32 + threadIdx.x;
    float sum = 0.f;

    for (int t = 0; t < p; t += TILE32) {
        As[threadIdx.y][threadIdx.x] = a[row * p + t + threadIdx.x];
        Bs[threadIdx.y][threadIdx.x] = b[(t + threadIdx.y) * n + col];
        __syncthreads();
        #pragma unroll
        for (int k = 0; k < TILE32; ++k)
            sum += As[threadIdx.y][k] * Bs[k][threadIdx.x];
        __syncthreads();
    }
    c[row * n + col] = sum;
}

// ===========================================================================
// 5. Tiled 32×32, 2D block, each thread 4 outputs — register-blocking.
//    Grid: (m/32, n/32), Block: 16×16. Each thread computes 2×2 outputs.
//    k-slice: 32.
// ===========================================================================
#define TILE32b 32
__global__ void mmTiled32Reg(float*       __restrict__ c,
                             const float* __restrict__ a,
                             const float* __restrict__ b,
                             int m, int n, int p) {
    __shared__ float As[TILE32b][TILE32b];
    __shared__ float Bs[TILE32b][TILE32b];

    int row0 = blockIdx.y * TILE32b + threadIdx.y * 2;
    int col0 = blockIdx.x * TILE32b + threadIdx.x * 2;

    float sum[2][2] = {{0,0},{0,0}};

    for (int t = 0; t < p; t += TILE32b) {
        // Load A: each thread loads 2 rows × 2 cols = 4 elements.
        As[threadIdx.y * 2 + 0][threadIdx.x * 2 + 0] = a[(row0 + 0) * p + t + threadIdx.x * 2 + 0];
        As[threadIdx.y * 2 + 0][threadIdx.x * 2 + 1] = a[(row0 + 0) * p + t + threadIdx.x * 2 + 1];
        As[threadIdx.y * 2 + 1][threadIdx.x * 2 + 0] = a[(row0 + 1) * p + t + threadIdx.x * 2 + 0];
        As[threadIdx.y * 2 + 1][threadIdx.x * 2 + 1] = a[(row0 + 1) * p + t + threadIdx.x * 2 + 1];

        // Load B: each thread loads 2 k-rows × 2 cols = 4 elements.
        Bs[threadIdx.y * 2 + 0][threadIdx.x * 2 + 0] = b[(t + threadIdx.y * 2 + 0) * n + col0 + 0];
        Bs[threadIdx.y * 2 + 0][threadIdx.x * 2 + 1] = b[(t + threadIdx.y * 2 + 0) * n + col0 + 1];
        Bs[threadIdx.y * 2 + 1][threadIdx.x * 2 + 0] = b[(t + threadIdx.y * 2 + 1) * n + col0 + 0];
        Bs[threadIdx.y * 2 + 1][threadIdx.x * 2 + 1] = b[(t + threadIdx.y * 2 + 1) * n + col0 + 1];

        __syncthreads();

        #pragma unroll
        for (int k = 0; k < TILE32b; ++k) {
            float a0 = As[threadIdx.y * 2 + 0][k];
            float a1 = As[threadIdx.y * 2 + 1][k];
            float b0 = Bs[k][threadIdx.x * 2 + 0];
            float b1 = Bs[k][threadIdx.x * 2 + 1];
            sum[0][0] += a0 * b0;
            sum[0][1] += a0 * b1;
            sum[1][0] += a1 * b0;
            sum[1][1] += a1 * b1;
        }

        __syncthreads();
    }

    c[(row0 + 0) * n + col0 + 0] = sum[0][0];
    c[(row0 + 0) * n + col0 + 1] = sum[0][1];
    c[(row0 + 1) * n + col0 + 0] = sum[1][0];
    c[(row0 + 1) * n + col0 + 1] = sum[1][1];
}

// ===========================================================================
// ADD MORE KERNELS HERE
// ===========================================================================
