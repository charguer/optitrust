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

    // const int dM = m / 32;
    const int dN = n / 32;

    const int bi = blockIdx.x / dN;         // block row  [0, dM)
    const int bj = blockIdx.x % dN;         // block col  [0, dN)
    const int tid = threadIdx.x;

    const int ti  = (tid % 32) / 8;        // thread row-group [0, 4)
    const int tj  = tid % 8;               // thread col       [0, 8)
    const int tjB = (tid % 32) / 4;        // B-load row-group [0, 8)
    const int kB  = tid % 4;               // B-load k         [0, 4)

    float sum[8][4];
    for (int i = 0; i < 8; ++i) {
        for (int j = 0; j < 4; ++j) {
          sum[i][j] = 0;
      }
    }

    for (int bkIdx = 0; bkIdx < p / 4; ++bkIdx) {
        // Load A tile (coalesced: consecutive tj → consecutive columns).
        a_smem[ti][0][tj] = a[(bi * 32 + ti * 8 + tj) * p + bkIdx * 4 + 0];
        a_smem[ti][1][tj] = a[(bi * 32 + ti * 8 + tj) * p + bkIdx * 4 + 1];
        a_smem[ti][2][tj] = a[(bi * 32 + ti * 8 + tj) * p + bkIdx * 4 + 2];
        a_smem[ti][3][tj] = a[(bi * 32 + ti * 8 + tj) * p + bkIdx * 4 + 3];

        // Load B tile.
        b_smem[tjB][kB][0] = b[(bkIdx * 4 + kB) * n + bj * 32 + tjB * 4 + 0];
        b_smem[tjB][kB][1] = b[(bkIdx * 4 + kB) * n + bj * 32 + tjB * 4 + 1];
        b_smem[tjB][kB][2] = b[(bkIdx * 4 + kB) * n + bj * 32 + tjB * 4 + 2];
        b_smem[tjB][kB][3] = b[(bkIdx * 4 + kB) * n + bj * 32 + tjB * 4 + 3];

        __syncthreads();

        // Accumulate: 4 k-steps × (8×4 outer product).
        for (int k = 0; k < 4; ++k) {
            float a_regs[8];
            float b_regs[4];

            a_regs[0] = a_smem[ti][k][0];
            a_regs[1] = a_smem[ti][k][1];
            a_regs[2] = a_smem[ti][k][2];
            a_regs[3] = a_smem[ti][k][3];
            a_regs[4] = a_smem[ti][k][4];
            a_regs[5] = a_smem[ti][k][5];
            a_regs[6] = a_smem[ti][k][6];
            a_regs[7] = a_smem[ti][k][7];

            b_regs[0] = b_smem[tj][k][0];
            b_regs[1] = b_smem[tj][k][1];
            b_regs[2] = b_smem[tj][k][2];
            b_regs[3] = b_smem[tj][k][3];

            #pragma unroll
            for (int i = 0; i < 8; ++i) {
              sum[i][0] += a_regs[i] * b_regs[0];
              sum[i][1] += a_regs[i] * b_regs[1];
              sum[i][2] += a_regs[i] * b_regs[2];
              sum[i][3] += a_regs[i] * b_regs[3];
            }
        }

        __syncthreads();
    }

    // Store 8×4 sub-tile.
    #pragma unroll
    for (int i = 0; i < 8; ++i) {
      c[(bi * 32 + ti * 8 + i) * n + bj * 32 + tj * 4 + 0] = sum[i][0];
      c[(bi * 32 + ti * 8 + i) * n + bj * 32 + tj * 4 + 1] = sum[i][1];
      c[(bi * 32 + ti * 8 + i) * n + bj * 32 + tj * 4 + 2] = sum[i][2];
      c[(bi * 32 + ti * 8 + i) * n + bj * 32 + tj * 4 + 3] = sum[i][3];
    }
}

__global__ void mmOptiTrust2DBlocktiling_1(float*       __restrict__ c,
                              const float* __restrict__ a,
                              const float* __restrict__ b,
                              int m, int n, int p) {
    __shared__ float As[32 * 4];
    __shared__ float Bs[4 * 32];

    const int dN = n / 32;
    const int bi = blockIdx.x / dN;
    const int bj = blockIdx.x % dN;
    const int tid = threadIdx.x;

    const int threadCol = tid % 8;
    const int threadRow = tid / 8;

    const int innerRowA = tid / 4;
    const int innerColA = tid % 4;
    const int strideA   = 32 / 4;

    const int innerRowB = 0;
    const int innerColB = tid;
    const int strideB   = 32 / 32;

    float sum[8][4];
    for (int i = 0; i < 8; ++i)
        for (int j = 0; j < 4; ++j)
            sum[i][j] = 0;

    float regM[8];
    float regN[4];

    for (int bkIdx = 0; bkIdx < p; bkIdx += 4) {
        for (int r = 0; r < 32 / strideA; ++r) {
            int row = r * strideA + innerRowA;
            As[row * 4 + innerColA] = a[(bi * 32 + row) * p + bkIdx + innerColA];
        }

        for (int r = 0; r < 4 / strideB; ++r) {
            int row = r * strideB + innerRowB;
            Bs[row * 32 + innerColB] = b[(row + bkIdx) * n + bj * 32 + innerColB];
        }

        __syncthreads();

        for (int k = 0; k < 4; ++k) {
            for (int i = 0; i < 8; ++i)
                regM[i] = As[(threadRow * 8 + i) * 4 + k];

            for (int j = 0; j < 4; ++j)
                regN[j] = Bs[k * 32 + threadCol * 4 + j];

            for (int i = 0; i < 8; ++i) {
              for (int j = 0; j < 4; j++) {
                sum[i][j] += regM[i] * regN[j];
              }
            }
        }

        __syncthreads();
    }

    for (int i = 0; i < 8; ++i) {
      for (int j = 0; j < 4; j++) {
        c[(bi * 32 + threadRow * 8 + i) * n + bj * 32 + threadCol * 4 + j] = sum[i][j];
      }
    }
}

__global__ void mmOptiTrust2DBlocktiling_2(float*       __restrict__ c,
                              const float* __restrict__ a,
                              const float* __restrict__ b,
                              int m, int n, int p) {
    __shared__ float As[64 * 32];
    __shared__ float Bs[32 * 64];

    const int dN = n / 64;
    const int bi = blockIdx.x / dN;
    const int bj = blockIdx.x % dN;
    const int tid = threadIdx.x;

    const int threadCol = tid % 32;
    const int threadRow = tid / 32;

    const int innerRowA = tid / 32;
    const int innerColA = tid % 32;
    const int strideA   = 1024 / 32;

    const int innerRowB = tid / 64;
    const int innerColB = tid % 64;
    const int strideB   = 1024 / 64;

    float sum[2][2];
    for (int i = 0; i < 2; ++i)
        for (int j = 0; j < 2; ++j)
            sum[i][j] = 0;

    float regM[2];
    float regN[2];

    for (int bkIdx = 0; bkIdx < p; bkIdx += 32) {
        for (int r = 0; r < 64 / strideA; ++r) {
            int row = r * strideA + innerRowA;
            As[row * 32 + innerColA] = a[(bi * 64 + row) * p + bkIdx + innerColA];
        }

        for (int r = 0; r < 32 / strideB; ++r) {
            int row = r * strideB + innerRowB;
            Bs[row * 64 + innerColB] = b[(row + bkIdx) * n + bj * 64 + innerColB];
        }

        __syncthreads();

        for (int k = 0; k < 32; ++k) {
            for (int i = 0; i < 2; ++i)
                regM[i] = As[(threadRow * 2 + i) * 32 + k];

            for (int j = 0; j < 2; ++j)
                regN[j] = Bs[k * 64 + threadCol * 2 + j];

            for (int i = 0; i < 2; ++i)
                for (int j = 0; j < 2; ++j)
                    sum[i][j] += regM[i] * regN[j];
        }

        __syncthreads();
    }

    for (int i = 0; i < 2; ++i)
        for (int j = 0; j < 2; ++j)
            c[(bi * 64 + threadRow * 2 + i) * n + bj * 64 + threadCol * 2 + j] = sum[i][j];
}

// ===========================================================================
// OptiTrust — 32×32 tile, 64 threads, k-slice 4
// Each thread computes a 4×4 sub-tile.
// ===========================================================================
__global__ void mmOptiTrust4x4(float*       __restrict__ c,
                               const float* __restrict__ a,
                               const float* __restrict__ b,
                               int m, int n, int p) {
    __shared__ float a_smem[8][4][4];  // [row_grp][k][row_in_grp]
    __shared__ float b_smem[8][4][4];  // [col_grp][k][col_in_grp]

    const int dN = n / 32;

    const int bi  = blockIdx.x / dN;
    const int bj  = blockIdx.x % dN;
    const int tid = threadIdx.x;         // 0..63

    const int ti  = tid / 8;             // row group  [0,8)
    const int tj  = tid % 8;             // col group  [0,8)

    // Load indices: each thread loads 2 elements of A and 2 of B.
    const int a_row = ti * 4 + (tj % 4); // row within block   [0,32)
    const int a_k   = (tj / 4) * 2;     // k offset (0 or 2)
    const int b_col = tj * 4 + (ti % 4); // col within block   [0,32)
    const int b_k   = (ti / 4) * 2;     // k offset (0 or 2)

    float sum[4][4];
    #pragma unroll
    for (int i = 0; i < 4; ++i)
        #pragma unroll
        for (int j = 0; j < 4; ++j)
            sum[i][j] = 0.f;

    for (int bkIdx = 0; bkIdx < p / 4; ++bkIdx) {
        // Load A: 2 elements per thread (row a_row, k a_k and a_k+1).
        a_smem[ti][a_k][tj % 4]       = a[(bi * 32 + a_row) * p + bkIdx * 4 + a_k];
        a_smem[ti][a_k + 1][tj % 4]   = a[(bi * 32 + a_row) * p + bkIdx * 4 + a_k + 1];

        // Load B: 2 elements per thread (k b_k and b_k+1, col b_col).
        b_smem[tj][b_k][ti % 4]       = b[(bkIdx * 4 + b_k)     * n + bj * 32 + b_col];
        b_smem[tj][b_k + 1][ti % 4]   = b[(bkIdx * 4 + b_k + 1) * n + bj * 32 + b_col];

        __syncthreads();

        // Accumulate: 4 k-steps × (4×4 outer product).
        #pragma unroll
        for (int k = 0; k < 4; ++k) {
            float ar[4] = {
                a_smem[ti][k][0], a_smem[ti][k][1],
                a_smem[ti][k][2], a_smem[ti][k][3]
            };
            float br[4] = {
                b_smem[tj][k][0], b_smem[tj][k][1],
                b_smem[tj][k][2], b_smem[tj][k][3]
            };
            #pragma unroll
            for (int i = 0; i < 4; ++i)
                #pragma unroll
                for (int j = 0; j < 4; ++j)
                    sum[i][j] += ar[i] * br[j];
        }

        __syncthreads();
    }

    // Store 4×4 sub-tile.
    #pragma unroll
    for (int i = 0; i < 4; ++i)
        #pragma unroll
        for (int j = 0; j < 4; ++j)
            c[(bi * 32 + ti * 4 + i) * n + bj * 32 + tj * 4 + j] = sum[i][j];
}
// ===========================================================================
// OptiTrust — 32×32 tile, 256 threads, k-slice 4
// Each thread computes a 2×2 sub-tile.
//   16×16 thread grid; thread (tRow,tCol) owns rows [2*tRow, 2*tRow+1]
//   and cols [2*tCol, 2*tCol+1].
// ===========================================================================
__global__ void mmOptiTrust2x2(float*       __restrict__ c,
                               const float* __restrict__ a,
                               const float* __restrict__ b,
                               int m, int n, int p) {
    __shared__ float a_smem[32][4];  // [row][k]  (128 floats)
    __shared__ float b_smem[4][32];  // [k][col]  (128 floats)

    const int dN = n / 32;

    const int bi  = blockIdx.x / dN;
    const int bj  = blockIdx.x % dN;
    const int tid = threadIdx.x;         // 0..255

    // Thread -> 2x2 output tile.
    const int tRow = tid / 16;           // [0,16)
    const int tCol = tid % 16;           // [0,16)

    float sum[2][2];
    #pragma unroll
    for (int i = 0; i < 2; ++i)
        #pragma unroll
        for (int j = 0; j < 2; ++j)
            sum[i][j] = 0.f;

    for (int bkIdx = 0; bkIdx < p / 4; ++bkIdx) {
        // Load phase: 256 threads, one element each.
        //   threads  0..127 -> A tile (row-major: row = e/4, k = e%4)
        //   threads 128..255 -> B tile (row-major: k = e/32, col = e%32)
        if (tid < 128) {
            int e = tid;
            a_smem[e / 4][e % 4] = a[(bi * 32 + e / 4) * p + bkIdx * 4 + (e % 4)];
        } else {
            int e = tid - 128;
            b_smem[e / 32][e % 32] = b[(bkIdx * 4 + (e / 32)) * n + bj * 32 + (e % 32)];
        }
        __syncthreads();

        // Accumulate: 4 k-steps × (2×2 outer product).
        #pragma unroll
        for (int k = 0; k < 4; ++k) {
            float ar[2] = { a_smem[tRow * 2 + 0][k],
                            a_smem[tRow * 2 + 1][k] };
            float br[2] = { b_smem[k][tCol * 2 + 0],
                            b_smem[k][tCol * 2 + 1] };
            #pragma unroll
            for (int i = 0; i < 2; ++i)
                #pragma unroll
                for (int j = 0; j < 2; ++j)
                    sum[i][j] += ar[i] * br[j];
        }

        __syncthreads();
    }

    // Store 2×2 sub-tile.
    #pragma unroll
    for (int i = 0; i < 2; ++i)
        #pragma unroll
        for (int j = 0; j < 2; ++j)
            c[(bi * 32 + tRow * 2 + i) * n + bj * 32 + tCol * 2 + j] = sum[i][j];
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

#define SIBOEHM6_GRID_DIM(rows, cols, BM, BN) ((rows)/BM * ((cols)/BN))
#define SIBOEHM6_BLOCK_DIM(BM, BN, TM, TN) (BM/TM * (BN/TN))
#define SIBOEHM6_SMEM(BM, BN, BK) (BM*BK + BK*BN) * sizeof(float_t)

template <int BM, int BN, int BK, int TM, int TN>
__global__
void mmSiboehm6(
    // float_t alpha,
    // float_t beta,
    float_t *gC,
    const float_t *gA,
    const float_t *gB,
    int rows, // m
    int cols, // n
    int shared // p
) {
    // shared memory cache
    // many hand written implementations would use:
    //  __shared__ float_t sA[BM*BK];
    //  __shared__ float_t sB[BK*BN];
    // but this leads to less flexibility in allocating shared memory
    // instead use dynamic shared memory with
    //   size in bytes == (BM*BK + BK*BN) * (sizeof(float_t))
    extern __shared__ float_t buffer[];
    float_t *sA = buffer;
    float_t *sB = &buffer[BM * BK];

    // thread local cache
    float_t rAcol[TM];
    float_t rBrow[TN];
    float_t rchProd[TM][TN];

    int num_n_tiles = cols / BN;
    // for each block, jump to beginning of tile row in gA
    int mrow = blockIdx.x / num_n_tiles;
    gA += shared * (mrow * BM);
    // for each block, jump to beginning of tile column in gB
    int mcol = blockIdx.x % num_n_tiles;
    gB += mcol * BN;
    // for each block, jump to the beginnin of the result tile in gC
    gC += mrow * BM * cols + mcol * BN;

    // row and column for each thread tile within the block tile
    int threadRow = threadIdx.x / (BN / TN);
    int threadCol = threadIdx.x % (BN / TN);

    for (int i = 0; i < TM; ++i) {
        for (int j = 0; j < TN; ++j) {
          rchProd[i][j] = 0;
      }
    }
    for (int bkIdx = 0; bkIdx < shared; bkIdx += BK) {
        __syncthreads();
        // load A vectorized and transposed into shared memory
        for (int i = 0; i < BM * BK; i += SIBOEHM6_BLOCK_DIM(BM, BN, TM, TN) * 4) {
            int row = (i + threadIdx.x * 4) / BK;
            int col = (i + threadIdx.x * 4) % BK;

            // COMMENT: Instead of using bkIdx in the indexing, we could make sure
            //   to advance gA and gB to the next blockTile every iteration.
            //     gA += BK;
            //     gB += BK * cols;
            //   In that case, bkIdx is never used anywhere. I find this unintuitive.
            float4 tmp = reinterpret_cast<const float4*>(&gA[bkIdx + shared * row + col])[0];
            sA[(col + 0) * BM + row] = tmp.x;
            sA[(col + 1) * BM + row] = tmp.y;
            sA[(col + 2) * BM + row] = tmp.z;
            sA[(col + 3) * BM + row] = tmp.w;
        }
        // load B vectorized into shared memory
        for (int i = 0; i < BK * BN; i += SIBOEHM6_BLOCK_DIM(BM, BN, TM, TN) * 4) {
            int row = (i + threadIdx.x * 4) / BN;
            int col = (i + threadIdx.x * 4) % BN;

            reinterpret_cast<float4*>(&sB[row * BN + col])[0] =
              reinterpret_cast<const float4*>(&gB[cols * bkIdx + cols * row + col])[0];
        }
        __syncthreads();

        // compute subproducts per thread tile
        for (int dotIdx = 0; dotIdx < BK; dotIdx++) {
            for (int i = 0; i < TM; i++) {
                rAcol[i] = sA[dotIdx * BM + threadRow * TM + i];
            }
            for (int i = 0; i < TN; i++) {
                rBrow[i] = sB[dotIdx * BN + threadCol * TN + i];
            }
            for (int resIdxM = 0; resIdxM < TM; resIdxM++) {
                for (int resIdxN = 0; resIdxN < TN; resIdxN++)
                    rchProd[resIdxM][resIdxN] +=
                        rAcol[resIdxM] * rBrow[resIdxN];
            }
        }
    }

    for (int resIdxM = 0; resIdxM < TM; resIdxM++) {
        for (int resIdxN = 0; resIdxN < TN; resIdxN++) {
            float_t *outElem =
              &gC[(threadRow * TM + resIdxM) * cols + threadCol * TN + resIdxN];
            outElem[0] = rchProd[resIdxM][resIdxN];
        }
    }
}


/**
 * Matrix multiplication (CUDA Kernel) on the device: C = A * B
 * wA is A's width and wB is B's width
 */
template <int BLOCK_SIZE> __global__ void mmCUDASample(
  float* __restrict__ C,
  const float* __restrict__ A,
  const float* __restrict__ B,
  int m, int wB, int wA)
{
    // Block index
    int bx = blockIdx.x;
    int by = blockIdx.y;

    // Thread index
    int tx = threadIdx.x;
    int ty = threadIdx.y;

    // Index of the first sub-matrix of A processed by the block
    int aBegin = wA * BLOCK_SIZE * by;

    // Index of the last sub-matrix of A processed by the block
    int aEnd = aBegin + wA - 1;

    // Step size used to iterate through the sub-matrices of A
    int aStep = BLOCK_SIZE;

    // Index of the first sub-matrix of B processed by the block
    int bBegin = BLOCK_SIZE * bx;

    // Step size used to iterate through the sub-matrices of B
    int bStep = BLOCK_SIZE * wB;

    // Csub is used to store the element of the block sub-matrix
    // that is computed by the thread
    float Csub = 0;

    // Loop over all the sub-matrices of A and B
    // required to compute the block sub-matrix
    for (int a = aBegin, b = bBegin; a <= aEnd; a += aStep, b += bStep) {
        // Declaration of the shared memory array As used to
        // store the sub-matrix of A
        __shared__ float As[BLOCK_SIZE][BLOCK_SIZE];

        // Declaration of the shared memory array Bs used to
        // store the sub-matrix of B
        __shared__ float Bs[BLOCK_SIZE][BLOCK_SIZE];

        // Load the matrices from device memory
        // to shared memory; each thread loads
        // one element of each matrix
        As[ty][tx] = A[a + wA * ty + tx];
        Bs[ty][tx] = B[b + wB * ty + tx];

        // Synchronize to make sure the matrices are loaded
        __syncthreads();

        // Multiply the two matrices together;
        // each thread computes one element
        // of the block sub-matrix
#pragma unroll
        for (int k = 0; k < BLOCK_SIZE; ++k) {
            Csub += As[ty][k] * Bs[k][tx];
        }

        // Synchronize to make sure that the preceding
        // computation is done before loading two new
        // sub-matrices of A and B in the next iteration
        __syncthreads();
    }

    // Write the block sub-matrix to device memory;
    // each thread writes one element
    int c               = wB * BLOCK_SIZE * by + BLOCK_SIZE * bx;
    C[c + wB * ty + tx] = Csub;
}

template <const int BM, const int BN, const int BK, const int TM, const int TN>
__global__ void __launch_bounds__((BM * BN) / (TM * TN), 1)
    mm2DBlocktiling(float*       __restrict__ C,
                    const float* __restrict__ A,
                    const float* __restrict__ B,
                    int M, int N, int K) {
  const uint cRow = blockIdx.y;
  const uint cCol = blockIdx.x;
  const uint totalResultsBlocktile = BM * BN;
  // A thread is responsible for calculating TM*TN elements in the blocktile
  const uint numThreadsBlocktile = totalResultsBlocktile / (TM * TN);
  // ResultsPerBlock / ResultsPerThread == ThreadsPerBlock
  // static_assert(numThreadsBlocktile == blockDim.x);
  // BN/TN are the number of threads to span a column
  const int threadCol = threadIdx.x % (BN / TN);
  const int threadRow = threadIdx.x / (BN / TN);
  // allocate space for the current blocktile in smem
  __shared__ float As[BM * BK];
  __shared__ float Bs[BK * BN];
  // Move blocktile to beginning of A's row and B's column
  A += cRow * BM * K;
  B += cCol * BN;
  C += cRow * BM * N + cCol * BN;
  // calculating the indices that this thread will load into SMEM
  const uint innerRowA = threadIdx.x / BK;
  const uint innerColA = threadIdx.x % BK;
  // calculates the number of rows of As that are being loaded in a single step
  // by a single block
  const uint strideA = numThreadsBlocktile / BK;
  const uint innerRowB = threadIdx.x / BN;
  const uint innerColB = threadIdx.x % BN;
  // for both As and Bs we want each load to span the full column-width, for
  // better GMEM coalescing (as opposed to spanning full row-width and iterating
  // across columns)
  const uint strideB = numThreadsBlocktile / BN;
  // allocate thread-local cache for results in registerfile
  float threadResults[TM * TN] = {0.0};
  // register caches for As and Bs
  float regM[TM] = {0.0};
  float regN[TN] = {0.0};
  // outer-most loop over block tiles
  for (uint bkIdx = 0; bkIdx < K; bkIdx += BK) {
    // populate the SMEM caches
    for (uint loadOffset = 0; loadOffset < BM; loadOffset += strideA) {
      As[(innerRowA + loadOffset) * BK + innerColA] =
          A[(innerRowA + loadOffset) * K + innerColA];
    }
    for (uint loadOffset = 0; loadOffset < BK; loadOffset += strideB) {
      Bs[(innerRowB + loadOffset) * BN + innerColB] =
          B[(innerRowB + loadOffset) * N + innerColB];
    }
    __syncthreads();
    // advance blocktile
    A += BK;     // move BK columns to right
    B += BK * N; // move BK rows down
    // calculate per-thread results
    for (uint dotIdx = 0; dotIdx < BK; ++dotIdx) {
      // block into registers
      for (uint i = 0; i < TM; ++i) {
        regM[i] = As[(threadRow * TM + i) * BK + dotIdx];
      }
      for (uint i = 0; i < TN; ++i) {
        regN[i] = Bs[dotIdx * BN + threadCol * TN + i];
      }
      for (uint resIdxM = 0; resIdxM < TM; ++resIdxM) {
        for (uint resIdxN = 0; resIdxN < TN; ++resIdxN) {
          threadResults[resIdxM * TN + resIdxN] +=
              regM[resIdxM] * regN[resIdxN];
        }
      }
    }
    __syncthreads();
  }
  // write out the results
  for (uint resIdxM = 0; resIdxM < TM; ++resIdxM) {
    for (uint resIdxN = 0; resIdxN < TN; ++resIdxN) {
      C[(threadRow * TM + resIdxM) * N + threadCol * TN + resIdxN] =
          threadResults[resIdxM * TN + resIdxN];
    }
  }
}
