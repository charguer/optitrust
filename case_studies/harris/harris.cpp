#include "harris.h"
#include <stdlib.h>
#include "../../include/optitrust.h"

void grayscale(float* out,
               int h, int w,
               const float* in)
{
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      out[MINDEX2(h, w, y, x)] = 0.299f * in[MINDEX3(3, h, w, 0, y, x)] +
                                 0.587f * in[MINDEX3(3, h, w, 1, y, x)] +
                                 0.114f * in[MINDEX3(3, h, w, 2, y, x)];
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
    // int r0 = (y + 0) * w;
    // int r1 = (y + 1) * w;
    // int r2 = (y + 2) * w;
    for (int x = 0; x < (w - n + 1); x++) {
      // int c0 = x + 0;
      // int c1 = x + 1;
      // int c2 = x + 2;
      float acc = 0.0f;
      for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
          acc += weights[MINDEX2(m, n, i, j)] * in[MINDEX2(h, w, y + i, x + j)];
        }
      }
      out[MINDEX2(h - m + 1, w - n + 1, y, x)] = acc;
    }
  }
}

void sobelX(float* out,
            int h, int w,
            const float* in)
{
  const float weights[3 * 3] = {
      -1.f/8.f, 0.f, 1.f/8.f,
      -2.f/8.f, 0.f, 2.f/8.f,
      -1.f/8.f, 0.f, 1.f/8.f
  };
  conv2D(out, h, w, in, 3, 3, weights);
}

void sobelY(float* out,
            int h, int w,
            const float* in)
{
  const float weights[3 * 3] = {
      -1.f/8.f, -2.f/8.f, -1.f/8.f,
       0.f/8.f,  0.f/8.f,  0.f/8.f,
       1.f/8.f,  2.f/8.f,  1.f/8.f
  };
  conv2D(out, h, w, in, 3, 3, weights);
}

void binomial(float* out,
              int h, int w,
              const float* in)
{
  const float weights[3 * 3] = {
      1.f/16.f, 2.f/16.f, 1.f/16.f,
      2.f/16.f, 4.f/16.f, 2.f/16.f,
      1.f/16.f, 2.f/16.f, 1.f/16.f
  };
  conv2D(out, h, w, in, 3, 3, weights);
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

void harris(float* out, int h, int w, const float* in, float kappa) {
  const int h1 = h - 2;
  const int w1 = w - 2;
  const int h2 = h1 - 2;
  const int w2 = w1 - 2;

  float* gray = (float*) MALLOC2(h, w, sizeof(float));
  float* ix = (float*) MALLOC2(h1, w1, sizeof(float));
  float* iy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* ixx = (float*) MALLOC2(h1, w1, sizeof(float));
  float* ixy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* iyy = (float*) MALLOC2(h1, w1, sizeof(float));
  float* sxx = (float*) MALLOC2(h2, w2, sizeof(float));
  float* sxy = (float*) MALLOC2(h2, w2, sizeof(float));
  float* syy = (float*) MALLOC2(h2, w2, sizeof(float));

  grayscale(gray, h, w, in);
  sobelX(ix, h, w, gray);
  sobelY(iy, h, w, gray);
  mul(ixx, h1, w1, ix, ix);
  mul(ixy, h1, w1, ix, iy);
  mul(iyy, h1, w1, iy, iy);
  binomial(sxx, h1, w1, ixx);
  binomial(sxy, h1, w1, ixy);
  binomial(syy, h1, w1, iyy);
  coarsity(out, h2, w2, sxx, sxy, syy, kappa);

  free(ix);
  free(iy);
  free(ixx);
  free(ixy);
  free(iyy);
  free(sxx);
  free(sxy);
  free(syy);
}