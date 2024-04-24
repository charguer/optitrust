#include <optitrust.h>
#include <stdlib.h>
#include "harris.h"
#include "omp.h"
// NOTE: using pretty matrix notation

int min(int a, int b) { return (a < b ? a : b); }

void harris(float* out, int h, int w, const float* in) {
#pragma omp parallel for
  for (int y = 0; y < -4 + h; y += 32) {
    float* iy = (float*)malloc(sizeof(float[4][-2 + w]));
    float* ix = (float*)malloc(sizeof(float[4][-2 + w]));
    float* gray = (float*)malloc(sizeof(float[4][w]));
    for (int y_out = 0; y_out < min(h, 36 + y) - y; y_out++) {
#pragma omp simd simdlen(8)
      for (int x = 0; x < w; x++) {
        gray[y_out % 4 * w + x] =
            0.298999994993f * in[(y_out + y) * w + x] +
            0.587000012398f * in[h * w + (y_out + y) * w + x] +
            0.11400000006f * in[2 * h * w + (y_out + y) * w + x];
      }
      if (2 <= y_out) {
        float acc_ix_v0 = 0.f;
        acc_ix_v0 += gray[(-2 + y_out) % 4 * w];
        acc_ix_v0 += 2.f * gray[(-1 + y_out) % 4 * w];
        acc_ix_v0 += gray[(y_out) % 4 * w];
        float acc_ix_v1 = 0.f;
        acc_ix_v1 += gray[(-2 + y_out) % 4 * w + 1];
        acc_ix_v1 += 2.f * gray[(-1 + y_out) % 4 * w + 1];
        acc_ix_v1 += gray[(y_out) % 4 * w + 1];
        float acc_ix_v2;
        float acc_iy_v0 = 0.f;
        acc_iy_v0 += -1.f * gray[(-2 + y_out) % 4 * w + 0];
        acc_iy_v0 += 1.f * gray[(y_out) % 4 * w + 0];
        float acc_iy_v1 = 0.f;
        acc_iy_v1 += -1.f * gray[(-2 + y_out) % 4 * w + 1];
        acc_iy_v1 += 1.f * gray[(y_out) % 4 * w + 1];
        float acc_iy_v2 = 0.f;
// TODO:
// #pragma omp simd simdlen(8)
        for (int x = 0; x < -2 + w; x++) {
          acc_ix_v2 = 0.f;
          acc_ix_v2 += gray[(-2 + y_out) % 4 * w + x + 2];
          acc_ix_v2 += 2.f * gray[(-1 + y_out) % 4 * w + x + 2];
          acc_ix_v2 += gray[(y_out) % 4 * w + x + 2];
          float acc_ix = 0.f;
          acc_ix += -0.0833333333333f * acc_ix_v0;
          acc_ix += 0.0833333333333f * acc_ix_v2;
          ix[(-2 + y_out) % 4 * (-2 + w) + x] = acc_ix;

          acc_iy_v2 = 0.f;
          acc_iy_v2 += -1.f * gray[(-2 + y_out) % 4 * w + x + 2];
          acc_iy_v2 += 1.f * gray[(y_out) % 4 * w + x + 2];
          float acc_iy = 0.f;
          acc_iy += 0.0833333333333f * acc_iy_v0;
          acc_iy += 0.166666666667f * acc_iy_v1;
          acc_iy += 0.0833333333333f * acc_iy_v2;
          iy[(-2 + y_out) % 4 * (-2 + w) + x] = acc_iy;

          acc_ix_v0 = acc_ix_v1;
          acc_ix_v1 = acc_ix_v2;
          acc_iy_v0 = acc_iy_v1;
          acc_iy_v1 = acc_iy_v2;
        }
      }
      if (4 <= y_out) {
        float ix_v0_0 = ix[(-4 + y_out) % 4 * (-2 + w)];
        float ix_v0_1 = ix[(-3 + y_out) % 4 * (-2 + w)];
        float ix_v0_2 = ix[(-2 + y_out) % 4 * (-2 + w)];
        float ix_v1_0 = ix[1 + (-4 + y_out) % 4 * (-2 + w)];
        float ix_v1_1 = ix[1 + (-3 + y_out) % 4 * (-2 + w)];
        float ix_v1_2 = ix[1 + (-2 + y_out) % 4 * (-2 + w)];
        float iy_v0_0 = iy[(-4 + y_out) % 4 * (-2 + w)];
        float iy_v0_1 = iy[(-3 + y_out) % 4 * (-2 + w)];
        float iy_v0_2 = iy[(-2 + y_out) % 4 * (-2 + w)];
        float iy_v1_0 = iy[1 + (-4 + y_out) % 4 * (-2 + w)];
        float iy_v1_1 = iy[1 + (-3 + y_out) % 4 * (-2 + w)];
        float iy_v1_2 = iy[1 + (-2 + y_out) % 4 * (-2 + w)];
        float acc_sxx_v0 = ix_v0_0 * ix_v0_0 + ix_v0_1 * ix_v0_1 + ix_v0_2 * ix_v0_2;
        float acc_sxx_v1 = ix_v1_0 * ix_v1_0 + ix_v1_1 * ix_v1_1 + ix_v1_2 * ix_v1_2;
        float acc_sxx_v2;
        float acc_sxy_v0 = ix_v0_0 * iy_v0_0 + ix_v0_1 * iy_v0_1 + ix_v0_2 * iy_v0_2;
        float acc_sxy_v1 = ix_v1_0 * iy_v1_0 + ix_v1_1 * iy_v1_1 + ix_v1_2 * iy_v1_2;
        float acc_sxy_v2;
        float acc_syy_v0 = iy_v0_0 * iy_v0_0 + iy_v0_1 * iy_v0_1 + iy_v0_2 * iy_v0_2;
        float acc_syy_v1 = iy_v1_0 * iy_v1_0 + iy_v1_1 * iy_v1_1 + iy_v1_2 * iy_v1_2;
        float acc_syy_v2;
// TODO:
// #pragma omp simd simdlen(8)
        for (int x = 0; x < -4 + w; x++) {
          float ix_v2_0 = ix[2 + (-4 + y_out) % 4 * (-2 + w) + x];
          float ix_v2_1 = ix[2 + (-3 + y_out) % 4 * (-2 + w) + x];
          float ix_v2_2 = ix[2 + (-2 + y_out) % 4 * (-2 + w) + x];
          float iy_v2_0 = iy[2 + (-4 + y_out) % 4 * (-2 + w) + x];
          float iy_v2_1 = iy[2 + (-3 + y_out) % 4 * (-2 + w) + x];
          float iy_v2_2 = iy[2 + (-2 + y_out) % 4 * (-2 + w) + x];
          acc_sxx_v2 = ix_v2_0 * ix_v2_0 + ix_v2_1 * ix_v2_1 + ix_v2_2 * ix_v2_2;
          acc_sxy_v2 = ix_v2_0 * iy_v2_0 + ix_v2_1 * iy_v2_1 + ix_v2_2 * iy_v2_2;
          acc_syy_v2 = iy_v2_0 * iy_v2_0 + iy_v2_1 * iy_v2_1 + iy_v2_2 * iy_v2_2;
          float acc_sxx = acc_sxx_v0 + acc_sxx_v1 + acc_sxx_v2;
          float acc_sxy = acc_sxy_v0 + acc_sxy_v1 + acc_sxy_v2;
          float acc_syy = acc_syy_v0 + acc_syy_v1 + acc_syy_v2;
          float det_out = acc_sxx * acc_syy - acc_sxy * acc_sxy;
          float trace_out = acc_sxx + acc_syy;
          out[(-4 + y_out + y) * (-4 + w) + x] =
              det_out - 0.0399999991059f * trace_out * trace_out;

          acc_sxx_v0 = acc_sxx_v1;
          acc_sxx_v1 = acc_sxx_v2;
          acc_sxy_v0 = acc_sxy_v1;
          acc_sxy_v1 = acc_sxy_v2;
          acc_syy_v0 = acc_syy_v1;
          acc_syy_v1 = acc_syy_v2;
        }
      }
    }
    free(gray);
    free(ix);
    free(iy);
  }
}
