#define NUM_REPS 1000

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

/*
    This version uses n/2 threads --
    it performs the first level of reduction when reading from global memory.
*/
template <class T> __global__ void reduce3(T *g_odata, T *g_idata, unsigned int n)
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

template <class T, unsigned int blockSize, bool nIsPow2> __global__ void reduce6(T *g_odata, T *g_idata, unsigned int n)
{
    // Handle to thread block group
    cg::thread_block cta   = cg::this_thread_block();
    T               *sdata = SharedMemory<T>();

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



 /* __global__ void __kernel0 (float* d_partial_sums, float* d_arr, int N) {
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
} */



#define OPTIGPU_KERNEL __kernel0
#define REF_KERNEL reduce3<float>
#define BEST_KERNEL(tpb) reduce6<float,tpb,true>

float reduce (float* arr, int N, int which_kernel)  {
  float sum = 0.f;
  float* const d_arr = __gmem_malloc1<float>(N);
  memcpy_host_to_device1(d_arr, arr, N);
  float* const partial_sums = new float[MSIZE1(exact_div(N, 512))];
  float* const d_partial_sums = __gmem_malloc1<float>(exact_div(N, 512));
  cudaEvent_t start, stop;

  cudaEventCreate(&start);
  cudaEventCreate(&stop);
  switch (which_kernel) {
    case 0:
      printf("Running OptiGPU kernel: \n");
      OPTIGPU_KERNEL<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
      break;
    case 1:
      printf("Running reduce3 (reference): \n");
      REF_KERNEL<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
      break;
    case 2:
      printf("Running reduce6 (best performing): \n");
      BEST_KERNEL(256)<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
      break;
  }

  cudaEventRecord(start, 0);
  for (int i = 0; i < NUM_REPS; i++) {
    switch (which_kernel) {
      case 0:
        OPTIGPU_KERNEL<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
        break;
      case 1:
        REF_KERNEL<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
        break;
      case 2:
        BEST_KERNEL(256)<<<MSIZE1(exact_div(N, 512)), MSIZE1(256), sizeof(float) * 256 + 0>>>(d_partial_sums, d_arr, N);
        break;
    }
  }
  cudaEventRecord(stop, 0);
  cudaEventSynchronize(stop);

  float kernelTime;
  cudaEventElapsedTime(&kernelTime, start, stop);

  float kernelBandwidth = 1000.0f * ((N + exact_div(N,512)) * sizeof(float)) / (1024 * 1024 * 1024) / (kernelTime / NUM_REPS);
        printf("Throughput = %.4f GB/s, Time = %.5f ms, "
               "elements = %u\n",
               kernelBandwidth,
               kernelTime / NUM_REPS,
               N);

  memcpy_device_to_host1(partial_sums, d_partial_sums, exact_div(N, 512));
  gmem_free(d_partial_sums);
   for (int bi = 0; bi < exact_div(N, 512); bi++) {
    sum = partial_sums[MINDEX1(exact_div(N, 512), bi)] + sum;
  }
  delete partial_sums;
  gmem_free(d_arr);
  return sum;
}
