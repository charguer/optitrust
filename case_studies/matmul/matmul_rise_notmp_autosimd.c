#include "matmul.h"
#include "omp.h"
#include <assert.h>

void parallel(float* output, float* x0, float* x1) {
  float t1[1048576];
#pragma omp parallel for
  for (int i0 = 0; i0 < 32; i0 = (1 + i0)) {
    for (int i1 = 0; i1 < 1024;i1 = (1 + i1)) {
      for (int i3 = 0; i3 < 32; i3 = (1 + i3)) {
        t1[((i3 + (32 * i1)) + (32768 * i0))] = x1[((i3 + (32 * i0)) + (1024 * i1))];
      }
    }
  }

#pragma omp parallel for
  for (int i4 = 0; i4 < 32; i4 = (1 + i4)) {
    for (int i5 = 0;(i5 < 32);i5 = (1 + i5)) {
      float t2[1024];
      for (int i6 = 0;(i6 < 32);i6 = (1 + i6)) {
        for (int i_50146 = 0;(i_50146 < 32);i_50146 = (1 + i_50146)) {
          t2[(i_50146 + (32 * i6))] = 0.0f;
        }
      }
    for (int i7 = 0;(i7 < 256);i7 = (1 + i7)) {
      for (int i8 = 0;(i8 < 32);i8 = (1 + i8)) {
        for (int i11 = 0; i11 < 32; i11 = (1 + i11)) {
          t2[(i11 + (32 * i8))] += x0[4*i7 + 1024*i8 + 32768*i4] * t1[i11 + 128*i7 + 32768*i5];
        }
        for (int i11 = 0; i11 < 32; i11 = (1 + i11)) {
          t2[(i11 + (32 * i8))] += x0[1 + 4*i7 + 1024*i8 + 32768*i4] * t1[32 + i11 + 128*i7 + 32768*i5];
        }
        for (int i11 = 0; i11 < 32; i11 = (1 + i11)) {
          t2[(i11 + (32 * i8))] += x0[2 + 4*i7 + 1024*i8 + 32768*i4] * t1[64 + i11 + 128*i7 + 32768*i5];
        }
        for (int i11 = 0; i11 < 32; i11 = (1 + i11)) {
          t2[(i11 + (32 * i8))] += x0[3 + 4*i7 + 1024*i8 + 32768*i4] * t1[96 + i11 + 128*i7 + 32768*i5];
        }
      }
    }

    for (int i13 = 0;(i13 < 32);i13 = (1 + i13)) {
      for (int i14 = 0;(i14 < 32);i14 = (1 + i14)) {
        output[(((i14 + (32 * i5)) + (1024 * i13)) + (32768 * i4))] = t2[(i14 + (32 * i13))];
      }
    }
} } }

void mm1024(float* C, float* A, float* B) {
    parallel(C, A, B);
}