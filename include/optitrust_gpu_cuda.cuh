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

int __smem_compute_size(int N) {
  return sizeof(float) * N; // TODO: convert to a template as well when it is fixed in optitrust_gpu.h
}


// TODO: stupid hack for now so that nvcc doesnt complain about these being actual functions
#define MINDEX0() (0)
#define MINDEX1(N1, N2) (i1)
#define MINDEX2(N1, N2, i1, i2) (i1*N2 + i2)
#define MINDEX3(N1, N2, N3, i1, i2, i3) ( i1 * N2 * N3 + i2 * N3 + i3)
#define MINDEX4(N1, N2, N3, N4, i1, i2, i3, i4) ( i1 * N2 * N3 * N4 + i2 * N3 * N4 + i3 * N4 + i4)
#define MINDEX5(N1, N2, N3, N4, N5, i1, i2, i3, i4, i5) ( i1 * N2 * N3 * N4 * N5 + i2 * N3 * N4 * N5 + i3 * N4 * N5 + i4 * N5 + i5)

#define DMINDEX0() (0)
#define DMINDEX1(N1, N2) (0)
#define DMINDEX2(N1, N2, i1, i2) (0)
#define DMINDEX3(N1, N2, N3, i1, i2, i3) (0)
#define DMINDEX4(N1, N2, N3, N4, i1, i2, i3, i4) (0)
#define DMINDEX5(N1, N2, N3, N4, N5, i1, i2, i3, i4, i5) (0)

#define MSIZE0() (1)
#define MSIZE1(N1) (N1)
#define MSIZE2(N1,N2) (N1*N2)
#define MSIZE3(N1,N2,N3) (N1*N2*N3)
#define MSIZE4(N1,N2,N3,N4) (N1*N2*N3*N4)
#define MSIZE5(N1,N2,N3,N4,N5) (N1*N2*N3*N4*N5)

#define MALLOC(T) (T*) malloc(sizeof(T))
#define MALLOC0(T) (T*) malloc(MSIZE0() * sizeof(T))
#define MALLOC1(T, N1) (T*) malloc(MSIZE1(N1) * sizeof(T))
#define MALLOC2(T, N1, N2) (T*) malloc(MSIZE2(N1, N2) * sizeof(T))
#define MALLOC3(T, N1, N2, N3) (T*) malloc(MSIZE3(N1, N2, N3) * sizeof(T))
#define MALLOC4(T, N1, N2, N3, N4) (T*) malloc(MSIZE4(N1, N2, N3, N4) * sizeof(T))
#define MALLOC5(T, N1, N2, N3, N4, N5) (T*) malloc(MSIZE4(N1, N2, N3, N4, N5) * sizeof(T))

#define CALLOC0(T) (T*) calloc(MSIZE0(), sizeof(T))
#define CALLOC1(T, N1) (T*) calloc(MSIZE1(N1), sizeof(T))
#define CALLOC2(T, N1, N2) (T*) calloc(MSIZE2(N1, N2), sizeof(T))
#define CALLOC3(T, N1, N2, N3) (T*) calloc(MSIZE3(N1, N2, N3), sizeof(T))
#define CALLOC4(T, N1, N2, N3, N4) (T*) calloc(MSIZE4(N1, N2, N3, N4), sizeof(T))
#define CALLOC5(T, N1, N2, N3, N4, N5) (T*) calloc(MSIZE5(N1, N2, N3, N4, N5), sizeof(T))

#endif // OPTITRUST_GPU_CUDA_CUH
