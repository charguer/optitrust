
#include <optitrust_gpu_cuda.cuh>






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

  void transpose (float* a, float* b, int W, int H)  {
  float* const d_a = __gmem_malloc2<float>(H, W);
  memcpy_host_to_device2(d_a, a, H, W);
  float* const d_b = __gmem_malloc2<float>(W, H);
  __kernel0<<<MSIZE2(exact_div(H, 32), exact_div(W, 32)), MSIZE2(16, 32), sizeof(float) * (
    32 * 32) + 0>>>(d_b, d_a, H, W);
  memcpy_device_to_host2(b, d_b, W, H);
  gmem_free(d_b);
  gmem_free(d_a);
}


