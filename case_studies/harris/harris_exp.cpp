#include <stdlib.h>

#include "../../include/optitrust.h"
#include "harris.h"
#include "omp.h"
// NOTE: using pretty matrix notation

int min(int a, int b) { return (a < b ? a : b); }

void grayscale(float* out, int h, int w, const float* in) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[y][x] = 0.298999994993f * in[0][y][x] +
                  0.587000012398f * in[1][y][x] + 0.11400000006f * in[2][y][x];
    }
  }
}

void conv2D(float* out, int h, int w, const float* in, int m, int n,
            const float* weights) {
  for (int y = 0; y < h - m + 1; y++) {
    for (int x = 0; x < w - n + 1; x++) {
      float acc = 0.f;
      for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
          acc += in[y + i][x + j] * weights[i][j];
        }
      }
      out[y][x] = acc;
    }
  }
}

const float weights_sobelX[3 * 3] = {-1.f / 12.f, 0.f, 1.f / 12.f,
                                     -2.f / 12.f, 0.f, 2.f / 12.f,
                                     -1.f / 12.f, 0.f, 1.f / 12.f};

void sobelX(float* out, int h, int w, const float* in) {
  conv2D(out, h, w, in, 3, 3, weights_sobelX);
}

const float weights_sobelY[3 * 3] = {-1.f / 12.f, -2.f / 12.f, -1.f / 12.f,
                                     0.f / 12.f,  0.f / 12.f,  0.f / 12.f,
                                     1.f / 12.f,  2.f / 12.f,  1.f / 12.f};

void sobelY(float* out, int h, int w, const float* in) {
  conv2D(out, h, w, in, 3, 3, weights_sobelY);
}

const float weights_sum3x3[3 * 3] = {1.f, 1.f, 1.f, 1.f, 1.f,
                                     1.f, 1.f, 1.f, 1.f};

void sum3x3(float* out, int h, int w, const float* in) {
  conv2D(out, h, w, in, 3, 3, weights_sum3x3);
}

void mul(float* out, int h, int w, const float* a, const float* b) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[y][x] = a[y][x] * b[y][x];
    }
  }
}

void coarsity(float* out, int h, int w, const float* sxx, const float* sxy,
              const float* syy, float kappa) {
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      float det = sxx[y][x] * syy[y][x] - sxy[y][x] * sxy[y][x];
      float trace = sxx[y][x] + syy[y][x];
      out[y][x] = det - kappa * trace * trace;
    }
  }
}

void harris(float* out, int h, int w, const float* in) {
#pragma omp parallel for
  for (int y = 0; y < -4 + h; y += 32) {
    float* const iy = (float* const)malloc(sizeof(float[4][-2 + w]));
    float* const ix = (float* const)malloc(sizeof(float[4][-2 + w]));
    float* const gray = (float* const)malloc(sizeof(float[4][w]));
    for (int y_out = 0; y_out < min(h, 36 + y) - y; y_out++) {
#pragma omp simd simdlen(8)
      for (int x = 0; x < w; x++) {
        gray[y_out % 4 * w + x] =
            0.298999994993f * in[(y_out + y) * w + x] +
            0.587000012398f * in[h * w + (y_out + y) * w + x] +
            0.11400000006f * in[2 * h * w + (y_out + y) * w + x];
      }
      if (2 <= y_out) {
#pragma omp simd simdlen(8)
        for (int x = 0; x < -2 + w; x++) {
          float acc_ix = 0.f;
          acc_ix += -0.0833333333333f * gray[(-2 + y_out) % 4 * w + x];
          acc_ix += 0.0833333333333f * gray[2 + (-2 + y_out) % 4 * w + x];
          acc_ix += -0.166666666667f * gray[(-1 + y_out) % 4 * w + x];
          acc_ix += 0.166666666667f * gray[2 + (-1 + y_out) % 4 * w + x];
          acc_ix += -0.0833333333333f * gray[y_out % 4 * w + x];
          acc_ix += 0.0833333333333f * gray[2 + y_out % 4 * w + x];
          ix[(-2 + y_out) % 4 * (-2 + w) + x] = acc_ix;
          float acc_iy = 0.f;
          acc_iy += -0.0833333333333f * gray[(-2 + y_out) % 4 * w + x];
          acc_iy += -0.166666666667f * gray[1 + (-2 + y_out) % 4 * w + x];
          acc_iy += -0.0833333333333f * gray[2 + (-2 + y_out) % 4 * w + x];
          acc_iy += 0.0833333333333f * gray[y_out % 4 * w + x];
          acc_iy += 0.166666666667f * gray[1 + y_out % 4 * w + x];
          acc_iy += 0.0833333333333f * gray[2 + y_out % 4 * w + x];
          iy[(-2 + y_out) % 4 * (-2 + w) + x] = acc_iy;
        }
      }
      if (4 <= y_out) {
#pragma omp simd simdlen(8)
        for (int x = 0; x < -4 + w; x++) {
          float ix0 = ix[(-4 + y_out) % 4 * (-2 + w) + x];
          float ix1 = ix[1 + (-4 + y_out) % 4 * (-2 + w) + x];
          float ix2 = ix[2 + (-4 + y_out) % 4 * (-2 + w) + x];
          float ix3 = ix[(-3 + y_out) % 4 * (-2 + w) + x];
          float ix4 = ix[1 + (-3 + y_out) % 4 * (-2 + w) + x];
          float ix5 = ix[2 + (-3 + y_out) % 4 * (-2 + w) + x];
          float ix6 = ix[(-2 + y_out) % 4 * (-2 + w) + x];
          float ix7 = ix[1 + (-2 + y_out) % 4 * (-2 + w) + x];
          float ix8 = ix[2 + (-2 + y_out) % 4 * (-2 + w) + x];
          float iy0 = iy[(-4 + y_out) % 4 * (-2 + w) + x];
          float iy1 = iy[1 + (-4 + y_out) % 4 * (-2 + w) + x];
          float iy2 = iy[2 + (-4 + y_out) % 4 * (-2 + w) + x];
          float iy3 = iy[(-3 + y_out) % 4 * (-2 + w) + x];
          float iy4 = iy[1 + (-3 + y_out) % 4 * (-2 + w) + x];
          float iy5 = iy[2 + (-3 + y_out) % 4 * (-2 + w) + x];
          float iy6 = iy[(-2 + y_out) % 4 * (-2 + w) + x];
          float iy7 = iy[1 + (-2 + y_out) % 4 * (-2 + w) + x];
          float iy8 = iy[2 + (-2 + y_out) % 4 * (-2 + w) + x];
          float acc_sxx = 0.f;
          acc_sxx += ix0 * ix0;
          acc_sxx += ix1 * ix1;
          acc_sxx += ix2 * ix2;
          acc_sxx += ix3 * ix3;
          acc_sxx += ix4 * ix4;
          acc_sxx += ix5 * ix5;
          acc_sxx += ix6 * ix6;
          acc_sxx += ix7 * ix7;
          acc_sxx += ix8 * ix8;
          float acc_sxy = 0.f;
          acc_sxy += ix0 * iy0;
          acc_sxy += ix1 * iy1;
          acc_sxy += ix2 * iy2;
          acc_sxy += ix3 * iy3;
          acc_sxy += ix4 * iy4;
          acc_sxy += ix5 * iy5;
          acc_sxy += ix6 * iy6;
          acc_sxy += ix7 * iy7;
          acc_sxy += ix8 * iy8;
          float acc_syy = 0.f;
          acc_syy += iy0 * iy0;
          acc_syy += iy1 * iy1;
          acc_syy += iy2 * iy2;
          acc_syy += iy3 * iy3;
          acc_syy += iy4 * iy4;
          acc_syy += iy5 * iy5;
          acc_syy += iy6 * iy6;
          acc_syy += iy7 * iy7;
          acc_syy += iy8 * iy8;
          float det_out = acc_sxx * acc_syy - acc_sxy * acc_sxy;
          float trace_out = acc_sxx + acc_syy;
          out[(-4 + y_out + y) * (-4 + w) + x] =
              det_out - 0.0399999991059f * trace_out * trace_out;
        }
      }
    }
    free(gray);
    free(ix);
    free(iy);
  }
}
