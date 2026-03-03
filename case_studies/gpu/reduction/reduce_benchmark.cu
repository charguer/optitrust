#define NUM_REPS 100

#include <cooperative_groups.h>
namespace cg = cooperative_groups;

// Utility class used to avoid linker errors with extern
// unsized shared memory arrays with templated type
template <class T> struct SharedMemory
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


template <class T> __global__ void reduce3(T *g_odata, T *g_idata, unsigned int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemory<T>();

    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = threadIdx.x;
    unsigned int i   = (blockIdx.x * blockDim.x + threadIdx.x) * 2;

    T mySum = g_idata[i] + g_idata[i+1];

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

template <class T> __global__ void reduce3_stride(T *g_odata, T *g_idata, unsigned int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemory<T>();

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


 __global__ void __kernel0 (float* d_partial_sums, float* d_arr, int N)  /*@*/{
  const int __ctx_sz = MSIZE1(exact_div(N, 256)) * MSIZE1(128);
  const int __tid = blockIdx.x * MSIZE1(128) + threadIdx.x;
  __shared__ float tile[MSIZE1(128)];
  const int __ctx_sz_0 = __ctx_sz / (exact_div(N, 256));
  const int __bi0 = __tid % __ctx_sz / __ctx_sz_0;
  const int __ctx_sz_1 = __ctx_sz_0 / 128;
  const int __ti1 = __tid % __ctx_sz_0 / __ctx_sz_1;
  const int __ctx_sz_2 = __ctx_sz_0 / (1 << 7);
  const int __t2 = __tid % __ctx_sz_0 / __ctx_sz_2;
  const int __ctx_sz_3 = __ctx_sz_0 / 128;
  const int __ti_f3 = __tid % __ctx_sz_0 / __ctx_sz_3;
  tile[MINDEX2(exact_div(N, 256), 128, 0, __ti1)] = 0.f;
  #pragma unroll
   for (int i = 0; i < 2; i++) {
    tile[MINDEX2(exact_div(N, 256), 128, 0, __ti1)] = tile[MINDEX2(exact_div(N, 256), 128, 0, __ti1)] + d_arr[MINDEX1(N, __bi0 * 256 + (
      __ti1 * 2 + i))];
  }
  __syncthreads();
   for (int s = 64; s > 0; s >>= 1) {
    if (__t2 < s) {
      // apparently doing
      // tile[__t2] = mySum = mySum + tile[s + __t2];
      // makes about a ~50gb/s difference in throughput..
      tile[__t2] += tile[s + __t2];
    }
    __syncthreads();
  }
  if (__ti_f3 == 0) {
    d_partial_sums[__bi0] = tile[0];
  }
}/*@*/

#define OPTIGPU_KERNEL __kernel0
#define REF_KERNEL reduce3<float>

float reduce (float* arr, int N)  {
  float sum = 0.f;
  float* const d_arr = __gmem_malloc1<float>(N);
  memcpy_host_to_device1(d_arr, arr, N);
  float* const partial_sums = new float[MSIZE1(exact_div(N, 256))];
  float* const d_partial_sums = __gmem_malloc1<float>(exact_div(N, 256));
  cudaEvent_t start, stop;

  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  OPTIGPU_KERNEL<<<MSIZE1(exact_div(N, 256)), MSIZE1(128), sizeof(float) * 128 + 0>>>(d_partial_sums, d_arr, N);
  cudaEventRecord(start, 0);
  for (int i = 0; i < NUM_REPS; i++) {
    OPTIGPU_KERNEL<<<MSIZE1(exact_div(N, 256)), MSIZE1(128), sizeof(float) * 128 + 0>>>(d_partial_sums, d_arr, N);
  }
  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  float kernelTime;
  cudaEventElapsedTime(&kernelTime, start, stop);

  float kernelBandwidth = 1000.0f * (N * sizeof(float)) / (1024 * 1024 * 1024) / (kernelTime / NUM_REPS);
        printf("Throughput = %.4f GB/s, Time = %.5f ms, "
               "elements = %u\n",
               kernelBandwidth,
               kernelTime / NUM_REPS,
               N);

  memcpy_device_to_host1(partial_sums, d_partial_sums, exact_div(N, 256));
  gmem_free(d_partial_sums);
   for (int bi = 0; bi < exact_div(N, 256); bi++) {
    sum = partial_sums[MINDEX1(exact_div(N, 256), bi)] + sum;
  }
  delete partial_sums;
  gmem_free(d_arr);
  return sum;
}
