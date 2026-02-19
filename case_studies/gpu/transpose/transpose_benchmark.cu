
#include <optitrust_gpu_cuda.cuh>
#include <stdio.h>

#include <cooperative_groups.h>

namespace cg = cooperative_groups;

#define BLOCK_ROWS 16
#define TILE_DIM 32

__global__ void transposeCoalesced(float *odata, float *idata, int height, int width)
{
    // Handle to thread block group
    cg::thread_block cta = cg::this_thread_block();
    __shared__ float tile[TILE_DIM][TILE_DIM];

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



 __global__ void __kernel0 (float* d_b, float* d_a, int H, int W)  /*@*/{
  const int __ctx_sz = MSIZE2(exact_div(H, 32), exact_div(W, 32)) * MSIZE2(16, 32);
  const int __tid = blockIdx.x * MSIZE2(16, 32) + threadIdx.x;
  __shared__ float tile[MSIZE2(32, 32)];
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
}/*@*/

#define NUM_REPS 100

#define OPTIGPU_KERNEL __kernel0<<<MSIZE2(exact_div(H, 32), exact_div(W, 32)), MSIZE2(16, 32), sizeof(float) * (32 * 32) + 0>>>
#define REF_KERNEL transposeCoalesced<<<dim3(exact_div(W,32),exact_div(H,32)),dim3(32,16)>>>

void transpose (float* a, float* b, int W, int H)  {
  float* const d_a = __gmem_malloc2<float>(H, W);
  memcpy_host_to_device2(d_a, a, H, W);
  float* const d_b = __gmem_malloc2<float>(W, H);


  cudaEvent_t start, stop;

  cudaEventCreate(&start);
  cudaEventCreate(&stop);

  OPTIGPU_KERNEL(d_b, d_a, H, W);
  cudaEventRecord(start, 0);
  for (int i = 0; i < NUM_REPS; i++) {
    OPTIGPU_KERNEL(d_b, d_a, H, W);
  }

  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);
  float kernelTime;
  cudaEventElapsedTime(&kernelTime, start, stop);

  float kernelBandwidth = 2.0f * 1000.0f * (W * H * sizeof(float)) / (1024 * 1024 * 1024) / (kernelTime / NUM_REPS);
        printf("Throughput = %.4f GB/s, Time = %.5f ms, "
               "elements = %u\n",
               kernelBandwidth,
               kernelTime / NUM_REPS,
               (W * H));

  memcpy_device_to_host2(b, d_b, W, H);
  gmem_free(d_b);
  gmem_free(d_a);
}


