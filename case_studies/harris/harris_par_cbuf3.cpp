#include <stdlib.h>

#include "../../include/optitrust.h"
#include "harris.h"

// NOTE: using pretty matrix notation
#include "omp.h"

int min(int a, int b) { return (a < b ? a : b); }

void harris(float* out, int h, int w, const float* in) {
#pragma omp parallel for
  for (int by = 0; by < -4 + h; by += 32) {
    float* gray = (float*)malloc(sizeof(float[3][w]));
    float* ix = (float*)malloc(sizeof(float[3][-2 + w]));
    float* iy = (float*)malloc(sizeof(float[3][-2 + w]));
    for (int y = by; y < min(h, 36 + by); y++) {
      if (by <= y) {
        for (int x = 0; x < w; x++) {
          gray[(y - by) % 3 * w + x] =
              0.298999994993 * in[0 * h * w + y * w + x] +
              0.587000012398 * in[h * w + y * w + x] +
              0.11400000006 * in[2 * h * w + y * w + x];
        }
      }
      if (2 + by <= y) {
        for (int x = 0; x < -2 + w; x++) {
          float acc_ix = 0.;
          acc_ix += -0.0833333333333 * gray[(-2 + y - by) % 3 * w + x];
          acc_ix += 0. * gray[1 + (-2 + y - by) % 3 * w + x];
          acc_ix += 0.0833333333333 * gray[2 + (-2 + y - by) % 3 * w + x];
          acc_ix += -0.166666666667 * gray[(-1 + y - by) % 3 * w + x];
          acc_ix += 0. * gray[1 + (-1 + y - by) % 3 * w + x];
          acc_ix += 0.166666666667 * gray[2 + (-1 + y - by) % 3 * w + x];
          acc_ix += -0.0833333333333 * gray[(y - by) % 3 * w + x];
          acc_ix += 0. * gray[1 + (y - by) % 3 * w + x];
          acc_ix += 0.0833333333333 * gray[2 + (y - by) % 3 * w + x];
          ix[(-2 + y - by) % 3 * (-2 + w) + x] = acc_ix;
          float acc_iy = 0.;
          acc_iy += -0.0833333333333 * gray[(-2 + y - by) % 3 * w + x];
          acc_iy += -0.166666666667 * gray[1 + (-2 + y - by) % 3 * w + x];
          acc_iy += -0.0833333333333 * gray[2 + (-2 + y - by) % 3 * w + x];
          acc_iy += 0. * gray[(-1 + y - by) % 3 * w + x];
          acc_iy += 0. * gray[1 + (-1 + y - by) % 3 * w + x];
          acc_iy += 0. * gray[2 + (-1 + y - by) % 3 * w + x];
          acc_iy += 0.0833333333333 * gray[(y - by) % 3 * w + x];
          acc_iy += 0.166666666667 * gray[1 + (y - by) % 3 * w + x];
          acc_iy += 0.0833333333333 * gray[2 + (y - by) % 3 * w + x];
          iy[(-2 + y - by) % 3 * (-2 + w) + x] = acc_iy;
        }
      }
      if (4 + by <= y) {
        for (int x = 0; x < -4 + w; x++) {
          float ix0 = ix[(-4 + y - by) % 3 * (-2 + w) + x];
          float ix1 = ix[1 + (-4 + y - by) % 3 * (-2 + w) + x];
          float ix2 = ix[2 + (-4 + y - by) % 3 * (-2 + w) + x];
          float ix3 = ix[(-3 + y - by) % 3 * (-2 + w) + x];
          float ix4 = ix[1 + (-3 + y - by) % 3 * (-2 + w) + x];
          float ix5 = ix[2 + (-3 + y - by) % 3 * (-2 + w) + x];
          float ix6 = ix[(-2 + y - by) % 3 * (-2 + w) + x];
          float ix7 = ix[1 + (-2 + y - by) % 3 * (-2 + w) + x];
          float ix8 = ix[2 + (-2 + y - by) % 3 * (-2 + w) + x];
          float iy0 = iy[(-4 + y - by) % 3 * (-2 + w) + x];
          float iy1 = iy[1 + (-4 + y - by) % 3 * (-2 + w) + x];
          float iy2 = iy[2 + (-4 + y - by) % 3 * (-2 + w) + x];
          float iy3 = iy[(-3 + y - by) % 3 * (-2 + w) + x];
          float iy4 = iy[1 + (-3 + y - by) % 3 * (-2 + w) + x];
          float iy5 = iy[2 + (-3 + y - by) % 3 * (-2 + w) + x];
          float iy6 = iy[(-2 + y - by) % 3 * (-2 + w) + x];
          float iy7 = iy[1 + (-2 + y - by) % 3 * (-2 + w) + x];
          float iy8 = iy[2 + (-2 + y - by) % 3 * (-2 + w) + x];
          float acc_sxx = 0.;
          acc_sxx += ix0 * ix0;
          acc_sxx += ix1 * ix1;
          acc_sxx += ix2 * ix2;
          acc_sxx += ix3 * ix3;
          acc_sxx += ix4 * ix4;
          acc_sxx += ix5 * ix5;
          acc_sxx += ix6 * ix6;
          acc_sxx += ix7 * ix7;
          acc_sxx += ix8 * ix8;
          float acc_sxy = 0.;
          acc_sxy += ix0 * iy0;
          acc_sxy += ix1 * iy1;
          acc_sxy += ix2 * iy2;
          acc_sxy += ix3 * iy3;
          acc_sxy += ix4 * iy4;
          acc_sxy += ix5 * iy5;
          acc_sxy += ix6 * iy6;
          acc_sxy += ix7 * iy7;
          acc_sxy += ix8 * iy8;
          float acc_syy = 0.;
          acc_syy += iy0 * iy0;
          acc_syy += iy1 * iy1;
          acc_syy += iy2 * iy2;
          acc_syy += iy3 * iy3;
          acc_syy += iy4 * iy4;
          acc_syy += iy5 * iy5;
          acc_syy += iy6 * iy6;
          acc_syy += iy7 * iy7;
          acc_syy += iy8 * iy8;
          float det = acc_sxx * acc_syy - acc_sxy * acc_sxy;
          float trace = acc_sxx + acc_syy;
          out[(-4 + y) * (-4 + w) + x] = det - 0.0399999991059 * trace * trace;
        }
      }
    }
    free(iy);
    free(ix);
    free(gray);
  }
}
