
#include <optitrust_gpu_cuda.cuh>






 __global__ void __kernel0 (float* d_b, float* d_a, float* d_c, int N)  /*@*/{
  const int __ctx_sz = MSIZE1(N / 256) * MSIZE1(256);
  const int __tid = blockIdx.x * MSIZE1(256) + threadIdx.x;
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __i0 = __tid % __ctx_sz / __ctx_sz_0;
  const float va = d_a[MINDEX1(N, __i0)];
  const float vb = d_b[MINDEX1(N, __i0)];
  d_c[MINDEX1(N, __i0)] = va + vb;
}/*@*/

  void vector_add (float* a, float* b, float* c, int N)  {
  float* const d_c = __gmem_malloc1<float>(N);
  float* const d_a = __gmem_malloc1<float>(N);
  memcpy_host_to_device1(d_a, a, N);
  float* const d_b = __gmem_malloc1<float>(N);
  memcpy_host_to_device1(d_b, b, N);
  __kernel0<<<MSIZE1(N / 256), MSIZE1(256), 0>>>(d_b, d_a, d_c, N);
  gmem_free(d_b);
  gmem_free(d_a);
  memcpy_device_to_host1(c, d_c, N);
  gmem_free(d_c);
}
