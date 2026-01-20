#ifndef OPTITRUST_GPU_CUDA_CUH
#define OPTITRUST_GPU_CUDA_CUH

#define OPTITRUST_CUDA_RUNTIME

#include "optitrust_common.h"
#include <cuda_runtime.h>

template <typename T> T* __gmem_malloc1(int N1) {
  T* p;
  cudaMalloc((void**)&p, N1 * sizeof(T)); // TODO handle error
  return p;
}

template <typename T> T* __gmem_malloc2(int N1, int N2) {
  T* p;
  cudaMalloc((void**)&p, N1 * N2 * sizeof(T)); // TODO handle error
  return p;
}

template <typename T> void gmem_free(T* p) {
  cudaFree(p); // TODO handle error
}

template <typename T> void memcpy_host_to_device1(T* dest, T* src, int N1) {
  cudaMemcpy((void*)dest, (const void*)src, N1 * sizeof(T), cudaMemcpyHostToDevice); // TODO handle error
}

template <typename T> void memcpy_host_to_device2(T* dest, T* src, int N1, int N2) {
  cudaMemcpy((void*)dest, (const void*)src, N1 * N2 * sizeof(T), cudaMemcpyHostToDevice); // TODO handle error
}

template <typename T> void memcpy_device_to_host1(T* dest, T* src, int N1) {
  cudaMemcpy((void*)dest, (const void*)src, N1 * sizeof(T), cudaMemcpyDeviceToHost); // TODO handle error
}

template <typename T> void memcpy_device_to_host2(T* dest, T* src, int N1, int N2) {
  cudaMemcpy((void*)dest, (const void*)src, N1 * N2 * sizeof(T), cudaMemcpyDeviceToHost); // TODO handle error
}


#endif // OPTITRUST_GPU_CUDA_CUH
