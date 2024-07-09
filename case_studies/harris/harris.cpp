#include <optitrust.h>
#include <stdlib.h>
#include "harris.h"

inline int min(int a, int b) { return a < b ? a : b; }

void grayscale(float* out,
               int h, int w,
               const float* in)
{
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] = 0.299f * in[MINDEX3(4, h, w, 0, y, x)] +
                                 0.587f * in[MINDEX3(4, h, w, 1, y, x)] +
                                 0.114f * in[MINDEX3(4, h, w, 2, y, x)];
    }
  }
}

// Computes a 2D convolution over `in` using `weights`.
// reads(in -> matrix2(h, w) * weights -> matrix2(m, n))
// writes(out -> matrix2(h - m + 1, w - n + 1))
//
// out[v] = sum_(d in indices(weights)) in[v + d] + weights[v]
// for y. for x. for a. for b.
void conv2D(float* out,
            int h, int w,
            const float* in,
            int m, int n,
            const float* weights)
{
  for (int y = 0; y < (h - m + 1); y++) {
    for (int x = 0; x < (w - n + 1); x++) {
      float acc = 0.0f;
      for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
          acc += in[MINDEX2(h, w, y + i, x + j)] * weights[MINDEX2(m, n, i, j)];
        }
      }
      out[MINDEX2(h - m + 1, w - n + 1, y, x)] = acc;
    }
  }
}

const float weights_sobelX[3 * 3] = {
    -1.f/12.f, 0.f, 1.f/12.f,
    -2.f/12.f, 0.f, 2.f/12.f,
    -1.f/12.f, 0.f, 1.f/12.f
};

void sobelX(float* out,
            int h, int w,
            const float* in)
{
  conv2D(out, h, w, in, 3, 3, weights_sobelX);
}

const float weights_sobelY[3 * 3] = {
    -1.f/12.f, -2.f/12.f, -1.f/12.f,
      0.f/12.f,  0.f/12.f,  0.f/12.f,
      1.f/12.f,  2.f/12.f,  1.f/12.f
};

void sobelY(float* out,
            int h, int w,
            const float* in)
{
  conv2D(out, h, w, in, 3, 3, weights_sobelY);
}

const float weights_sum3x3[3 * 3] = {
    1.f, 1.f, 1.f,
    1.f, 1.f, 1.f,
    1.f, 1.f, 1.f
};

void sum3x3(float* out,
            int h, int w,
            const float* in)
{
  conv2D(out, h, w, in, 3, 3, weights_sum3x3);
}

void mul(float* out,
         int h, int w,
         const float* a,
         const float* b)
{
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] = a[MINDEX2(h, w, y, x)] * b[MINDEX2(h, w, y, x)];
    }
  }
}

void coarsity(float* out,
              int h, int w,
              const float* sxx,
              const float* sxy,
              const float* syy,
              float kappa)
{
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      float det = sxx[MINDEX2(h, w, y, x)] * syy[MINDEX2(h, w, y, x)] - sxy[MINDEX2(h, w, y, x)] * sxy[MINDEX2(h, w, y, x)];
      float trace = sxx[MINDEX2(h, w, y, x)] + syy[MINDEX2(h, w, y, x)];
      out[MINDEX2(h, w, y, x)] = det - kappa * trace * trace;
    }
  }
}

void harris(float* out, int h, int w, const float* in) {
  const int h1 = h - 2;
  const int w1 = w - 2;
  const int h2 = h1 - 2;
  const int w2 = w1 - 2;

  float* const gray = (float*) MALLOC2(h, w, sizeof(float));
  float* const ix = (float*) MALLOC2(h1, w1, sizeof(float));
  float* const iy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* const ixx = (float*) MALLOC2(h1, w1, sizeof(float));
  float* const ixy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* const iyy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* const sxx = (float*) MALLOC2(h2, w2, sizeof(float));
  float* const sxy = (float*) MALLOC2(h2, w2, sizeof(float));
  float* const syy = (float*) MALLOC2(h2, w2, sizeof(float));

  grayscale(gray, h, w, in);
  sobelX(ix, h, w, gray);
  sobelY(iy, h, w, gray);
  mul(ixx, h1, w1, ix, ix);
  mul(ixy, h1, w1, ix, iy);
  mul(iyy, h1, w1, iy, iy);
  sum3x3(sxx, h1, w1, ixx);
  sum3x3(sxy, h1, w1, ixy);
  sum3x3(syy, h1, w1, iyy);
  coarsity(out, h2, w2, sxx, sxy, syy, 0.04f);

  MFREE2(h, w, gray);
  MFREE2(h1, w1, ix);
  MFREE2(h1, w1, iy);
  MFREE2(h1, w1, ixx);
  MFREE2(h1, w1, ixy);
  MFREE2(h1, w1, iyy);
  MFREE2(h2, w2, sxx);
  MFREE2(h2, w2, sxy);
  MFREE2(h2, w2, syy);
}
