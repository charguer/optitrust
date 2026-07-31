
#include <optitrust_gpu_cuda.cuh>

const int bm = 32;

const int bn = 32;

const int bk = 4;

const int tn = 4;

const int tm = 8;

__global__ void __kernel0(float* b_gmem, float* a_gmem, float* c_gmem, int p,
                          int n, int m) {
  const int __ctx_sz = MSIZE2(exact_div(m, 32), exact_div(n, 32)) *
                       MSIZE2(exact_div(32, 8), exact_div(32, 4));
  const int __tid =
      blockIdx.x * MSIZE2(exact_div(32, 8), exact_div(32, 4)) + threadIdx.x;
  SharedMemory smem;
  float* const b_smem = (float*)smem.ptr(MSIZE3(8, 4, 4));
  float* const a_smem = (float*)smem.ptr(MSIZE3(4, 4, 8));
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
  float* const sum = __treg_ref_uninit2<float>(8, 4);
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
    for (int i = 0; i < 8; i++) {
      for (int j = 0; j < 4; j++) {
      }
    }
     for (int k = 0; k < 4; k++) {
      float* const a_regs = __treg_ref_uninit1_s<float>(8);
       for (int i = 0; i < 8; i++) {
        a_regs[MINDEX1(8, i)] = a_smem[MINDEX4(
            exact_div(m, 32) * (exact_div(n, 32)), 4, 4, 8, 0, __ti8, k, i)];
      }
      float* const b_regs = __treg_ref_uninit1_s<float>(4);
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
    for (int i = 0; i < 8; i++) {
      for (int j = 0; j < 4; j++) {
      }
    }
  }
   for (int i = 0; i < 8; i++) {
     for (int j = 0; j < 4; j++) {
      c_gmem[MINDEX2(m, n, __bi0 * 32 + (__ti10 * 8 + i),
                     __bj1 * 32 + (__tj11 * 4 + j))] =
          sum[MINDEX3(4 * 8, 8, 4, 0, i, j)];
    }
  }
}

void mm(float* c, float* a, float* b, int m, int n, int p) {
  float* const c_gmem = __gmem_malloc2<float>(m, n);
  float* const a_gmem = __gmem_malloc2<float>(m, p);
  memcpy_host_to_device2(a_gmem, a, m, p);
  float* const b_gmem = __gmem_malloc2<float>(p, n);
  memcpy_host_to_device2(b_gmem, b, p, n);
  __kernel0<<<MSIZE2(exact_div(m, 32), exact_div(n, 32)),
              MSIZE2(exact_div(32, 8), exact_div(32, 4)),
              sizeof(float) * (4 * 4 * 8) +
                  (sizeof(float) * (8 * 4 * 4) + 0)>>>(b_gmem, a_gmem, c_gmem,
                                                       p, n, m);
  gmem_free(b_gmem);
  gmem_free(a_gmem);
  memcpy_device_to_host2(c, c_gmem, m, n);
  gmem_free(c_gmem);
}
