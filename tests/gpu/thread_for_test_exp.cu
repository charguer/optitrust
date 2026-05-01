
#include <optitrust_gpu_cuda.cuh>






 __device__ void basic (int __ctx_sz, int __tid, int* a, int N, int M)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __i0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / M;
  const int __j1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  a[MINDEX2(N, M, __i0, __j1)] = 1;
  __syncthreads();
}

 __device__ void retile_desyncgroups (int __ctx_sz, int __tid, int* a, int N, int M
)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __i0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / M;
  const int __j1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz / (N * M / 32);
  const int __i2 = __tid % __ctx_sz / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_2 / 32;
  const int __j3 = __tid % __ctx_sz_2 / __ctx_sz_3;
  a[__i0 * M + __j1] = 1;
  const int va = a[__i2 * 32 + __j3];
  a[__i2 * 32 + __j3] = va + 1;
  __syncthreads();
}

 __device__ void sync_required (int __ctx_sz, int __tid, int* a, int N, int M
)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __i0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / M;
  const int __j1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz / M;
  const int __i2 = __tid % __ctx_sz / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_2 / N;
  const int __j3 = __tid % __ctx_sz_2 / __ctx_sz_3;
  a[MINDEX2(N, M, __i0, __j1)] = 1;
  __syncthreads();
  a[MINDEX2(N, M, __j3, __i2)] = a[MINDEX2(N, M, __j3, __i2)] + 1;
  __syncthreads();
}

 __device__ void write_test1 (int __ctx_sz, int __tid, int* a, int N)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __i0 = __tid % __ctx_sz / __ctx_sz_0;
  a[__i0] = 1;
}

 __device__ void write_test2 (int __ctx_sz, int __tid, int* a, int N)  {
  write_test1(__ctx_sz, __tid, a, N);
  __syncthreads();
}

 __device__ void read_thread_outer (int __ctx_sz, int __tid, int* a, int* b, int N
)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __t0 = __tid % __ctx_sz / __ctx_sz_0;
  a[__t0] = 0;
   for (int i = 0; i < N; i++) {
    const int va = a[__t0];
    const int vb = b[MINDEX1(N, i)];
    a[__t0] = va + vb;
  }
}

 __device__ void read_thread_inner (int __ctx_sz, int __tid, int* a, int* b, int N
)  {
  const int __ctx_sz_0 = __ctx_sz / N;
  const int __t0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz / N;
  const int __t1 = __tid % __ctx_sz / __ctx_sz_1;
  a[MINDEX1(N, __t0)] = 0;
   for (int i = 0; i < N; i++) {
    const int va = a[MINDEX1(N, __t1)];
    const int vb = b[MINDEX1(N, i)];
    a[MINDEX1(N, __t1)] = va + vb;
  }
}


