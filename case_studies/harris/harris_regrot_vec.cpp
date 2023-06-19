#include <stdlib.h>

#include "../../include/optitrust.h"
#include <stdint.h>
#include "harris.h"
#include "omp.h"
// NOTE: using pretty matrix notation

#include <stdio.h>

int min(int a, int b) { return (a < b ? a : b); }

typedef float float4 __attribute__((__vector_size__(16)));
typedef float float8 __attribute__((__vector_size__(32)));

void harris(float* out, int h, int w, const float* s_in) {
  assert(w % 8 == 0);
  assert((w - 4) % 4 == 0);
  assert(((uintptr_t)s_in) % 32 == 0);
  const float8* in = (const float8*) s_in;
  // FIXME: assert(((uintptr_t)s_out) % 16 == 0);
  // float4* out = (float4*) s_out;
  const int iw = w/8;
#pragma omp parallel for
  for (int y = 0; y < -4 + h; y += 32) {
    float8* iy = (float8*)malloc(sizeof(float8[4][iw])); // + 2 padding
    float8* ix = (float8*)malloc(sizeof(float8[4][iw])); // + 2 padding
    const int gw = (w + 8) / 8; // + 8 padding
    float8* gray = (float8*)malloc(sizeof(float8[4][gw]));
    for (int y_out = 0; y_out < min(h, 36 + y) - y; y_out++) {
      for (int x = 0; x < iw; x++) {
        gray[y_out % 4 * gw + x] =
            0.298999994993f * in[(y_out + y) * iw + x] +
            0.587000012398f * in[h * iw + (y_out + y) * iw + x] +
            0.11400000006f * in[2 * h * iw + (y_out + y) * iw + x];
      }
      if (2 <= y_out) {
        float8 acc_ix_v0 =
                gray[(-2 + y_out) % 4 * gw] +
          2.f * gray[(-1 + y_out) % 4 * gw] +
                gray[y_out % 4 * gw];
        float8 acc_ix_v1;
        float8 acc_iy_v0 =
          -1.f * gray[(-2 + y_out) % 4 * gw] +
           1.f * gray[y_out % 4 * gw];
        float8 acc_iy_v1;
        for (int x = 0; x < iw; x++) {
          acc_ix_v1 =
                  gray[(-2 + y_out) % 4 * gw + x + 1] +
            2.f * gray[(-1 + y_out) % 4 * gw + x + 1] +
                  gray[y_out % 4 * gw + x + 1];
          float8 perm_ix_v0 = acc_ix_v0;
          float8 perm_ix_v2 = __builtin_shufflevector(acc_ix_v0, acc_ix_v1,
            2, 3, 4, 5, 6, 7, 8, 9);
          float8 acc_ix =
            -0.0833333333333f * perm_ix_v0 +
             0.0833333333333f * perm_ix_v2;
          ix[(-2 + y_out) % 4 * iw + x] = acc_ix;

          acc_iy_v1 =
            -1.f * gray[(-2 + y_out) % 4 * gw + x + 1] +
             1.f * gray[y_out % 4 * gw + x + 1];
          float8 perm_iy_v0 = acc_iy_v0;
          float8 perm_iy_v1 = __builtin_shufflevector(acc_iy_v0, acc_iy_v1,
            1, 2, 3, 4, 5, 6, 7, 8);
          float8 perm_iy_v2 = __builtin_shufflevector(acc_iy_v0, acc_iy_v1,
            2, 3, 4, 5, 6, 7, 8, 9);
          float8 acc_iy =
            0.0833333333333f * perm_iy_v0 +
            0.166666666667f * perm_iy_v1 +
            0.0833333333333f * perm_iy_v2;
          iy[(-2 + y_out) % 4 * iw + x] = acc_iy;

          acc_ix_v0 = acc_ix_v1;
          acc_iy_v0 = acc_iy_v1;
        }
      }
      if (4 <= y_out) {
        float8 ix_v0_0 = ix[(-4 + y_out) % 4 * iw];
        float8 ix_v0_1 = ix[(-3 + y_out) % 4 * iw];
        float8 ix_v0_2 = ix[(-2 + y_out) % 4 * iw];
        float8 iy_v0_0 = iy[(-4 + y_out) % 4 * iw];
        float8 iy_v0_1 = iy[(-3 + y_out) % 4 * iw];
        float8 iy_v0_2 = iy[(-2 + y_out) % 4 * iw];
        float8 acc_sxx_v0 = ix_v0_0 * ix_v0_0 + ix_v0_1 * ix_v0_1 + ix_v0_2 * ix_v0_2;
        float8 acc_sxx_v1;
        float8 acc_sxy_v0 = ix_v0_0 * iy_v0_0 + ix_v0_1 * iy_v0_1 + ix_v0_2 * iy_v0_2;
        float8 acc_sxy_v1;
        float8 acc_syy_v0 = iy_v0_0 * iy_v0_0 + iy_v0_1 * iy_v0_1 + iy_v0_2 * iy_v0_2;
        float8 acc_syy_v1;
// TODO:
// #pragma omp simd simdlen(8)
        for (int x = 0; x < (w-8)/8; x++) {
          float8 ix_v1_0 = ix[1 + (-4 + y_out) % 4 * iw + x];
          float8 ix_v1_1 = ix[1 + (-3 + y_out) % 4 * iw + x];
          float8 ix_v1_2 = ix[1 + (-2 + y_out) % 4 * iw + x];
          float8 iy_v1_0 = iy[1 + (-4 + y_out) % 4 * iw + x];
          float8 iy_v1_1 = iy[1 + (-3 + y_out) % 4 * iw + x];
          float8 iy_v1_2 = iy[1 + (-2 + y_out) % 4 * iw + x];
          acc_sxx_v1 = ix_v1_0 * ix_v1_0 + ix_v1_1 * ix_v1_1 + ix_v1_2 * ix_v1_2;
          acc_sxy_v1 = ix_v1_0 * iy_v1_0 + ix_v1_1 * iy_v1_1 + ix_v1_2 * iy_v1_2;
          acc_syy_v1 = iy_v1_0 * iy_v1_0 + iy_v1_1 * iy_v1_1 + iy_v1_2 * iy_v1_2;
          float8 perm_sxx_v0 = acc_sxx_v0;
          float8 perm_sxx_v1 = __builtin_shufflevector(acc_sxx_v0, acc_sxx_v1,
            1, 2, 3, 4, 5, 6, 7, 8);
          float8 perm_sxx_v2 = __builtin_shufflevector(acc_sxx_v0, acc_sxx_v1,
            2, 3, 4, 5, 6, 7, 8, 9);
          float8 perm_sxy_v0 = acc_sxy_v0;
          float8 perm_sxy_v1 = __builtin_shufflevector(acc_sxy_v0, acc_sxy_v1,
            1, 2, 3, 4, 5, 6, 7, 8);
          float8 perm_sxy_v2 = __builtin_shufflevector(acc_sxy_v0, acc_sxy_v1,
            2, 3, 4, 5, 6, 7, 8, 9);
          float8 perm_syy_v0 = acc_syy_v0;
          float8 perm_syy_v1 = __builtin_shufflevector(acc_syy_v0, acc_syy_v1,
            1, 2, 3, 4, 5, 6, 7, 8);
          float8 perm_syy_v2 = __builtin_shufflevector(acc_syy_v0, acc_syy_v1,
            2, 3, 4, 5, 6, 7, 8, 9);
          float8 acc_sxx = perm_sxx_v0 + perm_sxx_v1 + perm_sxx_v2;
          float8 acc_sxy = perm_sxy_v0 + perm_sxy_v1 + perm_sxy_v2;
          float8 acc_syy = perm_syy_v0 + perm_syy_v1 + perm_syy_v2;
          float8 det_out = acc_sxx * acc_syy - acc_sxy * acc_sxy;
          float8 trace_out = acc_sxx + acc_syy;
          float8 res = det_out - 0.0399999991059f * trace_out * trace_out;
          // FIXME:?
          out[(-4 + y_out + y) * (-4 + w) + x * 8] = res[0];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 1] = res[1];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 2] = res[2];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 3] = res[3];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 4] = res[4];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 5] = res[5];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 6] = res[6];
          out[(-4 + y_out + y) * (-4 + w) + x * 8 + 7] = res[7];

          acc_sxx_v0 = acc_sxx_v1;
          acc_sxy_v0 = acc_sxy_v1;
          acc_syy_v0 = acc_syy_v1;
        }
        // vector epilogue
        { // if-guard?
          // FIXME: epilogue is bugged
          float4 perm_sxx_v0 = __builtin_shufflevector(acc_sxx_v0, acc_sxx_v0,
            0, 1, 2, 3);
          float4 perm_sxx_v1 = __builtin_shufflevector(acc_sxx_v0, acc_sxx_v0,
            1, 2, 3, 4);
          float4 perm_sxx_v2 = __builtin_shufflevector(acc_sxx_v0, acc_sxx_v0,
            2, 3, 4, 5);
          float4 perm_sxy_v0 = __builtin_shufflevector(acc_sxy_v0, acc_sxy_v0,
            0, 1, 2, 3);
          float4 perm_sxy_v1 = __builtin_shufflevector(acc_sxy_v0, acc_sxy_v0,
            1, 2, 3, 4);
          float4 perm_sxy_v2 = __builtin_shufflevector(acc_syy_v0, acc_syy_v0,
            2, 3, 4, 5);
          float4 perm_syy_v0 = __builtin_shufflevector(acc_syy_v0, acc_syy_v0,
            0, 1, 2, 3);
          float4 perm_syy_v1 = __builtin_shufflevector(acc_syy_v0, acc_syy_v0,
            1, 2, 3, 4);
          float4 perm_syy_v2 = __builtin_shufflevector(acc_syy_v0, acc_syy_v0,
            2, 3, 4, 5);
          float4 acc_sxx = perm_sxx_v0 + perm_sxx_v1 + perm_sxx_v2;
          float4 acc_sxy = perm_sxy_v0 + perm_sxy_v1 + perm_sxy_v2;
          float4 acc_syy = perm_syy_v0 + perm_syy_v1 + perm_syy_v2;
          float4 det_out = acc_sxx * acc_syy - acc_sxy * acc_sxy;
          float4 trace_out = acc_sxx + acc_syy;
          float4 res = det_out - 0.0399999991059f * trace_out * trace_out;
          out[(-4 + y_out + y) * (-4 + w) + w - 8] = res[0];
          out[(-4 + y_out + y) * (-4 + w) + w - 7] = res[1];
          out[(-4 + y_out + y) * (-4 + w) + w - 6] = res[2];
          out[(-4 + y_out + y) * (-4 + w) + w - 5] = res[3];
        }
      }
    }
    free(gray);
    free(ix);
    free(iy);
  }
}
