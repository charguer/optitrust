
#include <optitrust_gpu_cuda.cuh>






 __global__ void __kernel0 (float* d_partial_sums, float* d_arr, int N)  /*@*/{
  const int __ctx_sz = MSIZE1(exact_div(N, 512)) * MSIZE1(256);
  const int __tid = blockIdx.x * MSIZE1(256) + threadIdx.x;
  __shared__ float tile[MSIZE1(256)];
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
  float* const reduce_arr_1 = &tile[MINDEX2(exact_div(N, 512), 256, 0, 0)];
   for (int t = 0; t < 1 << 8; t++) {  }
   for (int i = 8; i > 0; i--) {
    const int ei = 1 << i - 1;
    const int eii = 1 << i;
    if (__t2 < 1 << i - 1) {
      reduce_arr_1[MINDEX1(1 << 8, __t2)] = reduce_arr_1[MINDEX1(1 << 8, __t2)] + (
        &reduce_arr_1[ei])[MINDEX1(ei, __t2)];
    }
    else {  }
    __syncthreads();
  }
  if (__ti_f3 == 0) {
    const float sum_temp_2 = reduce_arr_1[MINDEX1(1 << 8, 0)];
     for (int t = 0; t < 1 << 8; t++) {  }
    d_partial_sums[MINDEX1(exact_div(N, 512), __bi0)] = sum_temp_2;
  }
  else {  }
}/*@*/

  float reduce (float* arr, int N)  {
  float sum = 0.f;
  float* const d_arr = __gmem_malloc1<float>(N);
  memcpy_host_to_device1(d_arr, arr, N);
  float* const partial_sums = new float[MSIZE1(exact_div(N, 512))];
  float* const d_partial_sums = __gmem_malloc1<float>(exact_div(N, 512));
  __kernel0<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
  memcpy_device_to_host1(partial_sums, d_partial_sums, exact_div(N, 512));
  gmem_free(d_partial_sums);
   for (int bi = 0; bi < exact_div(N, 512); bi++) {
    sum = partial_sums[MINDEX1(exact_div(N, 512), bi)] + sum;
  }
  delete partial_sums;
  gmem_free(d_arr);
  return sum;
}
