#include <stdlib.h>

#include "../../include/optitrust.h"
#include "harris.h"

// NOTE: using pretty matrix notation
#include "omp.h"

void harris(float* out, int h, int w, const float* in, float kappa) {
  float* gray = (float*)malloc(sizeof(float[h][w]));
  float* ix = (float*)malloc(sizeof(float[h - 2][w - 2]));
  float* iy = (float*)malloc(sizeof(float[h - 2][w - 2]));
  float* ixx = (float*)malloc(sizeof(float[h - 2][w - 2]));
  float* ixy = (float*)malloc(sizeof(float[h - 2][w - 2]));
  float* iyy = (float*)malloc(sizeof(float[h - 2][w - 2]));
  float* sxx = (float*)malloc(sizeof(float[h - 2 - 2][w - 2 - 2]));
  float* sxy = (float*)malloc(sizeof(float[h - 2 - 2][w - 2 - 2]));
  float* syy = (float*)malloc(sizeof(float[h - 2 - 2][w - 2 - 2]));
  for (int y = 0; y < h; y++) {
    for (int x = 0; x < w; x++) {
      gray[y][x] = 0.298999994993 * in[0][y][x] + 0.587000012398 * in[1][y][x] +
                   0.11400000006 * in[2][y][x];
    }
  }
  for (int y = 0; y < h - 3 + 1; y++) {
    for (int x = 0; x < w - 3 + 1; x++) {
      float acc = 0.;
      acc += -1. / 8. * gray[y + 0][x + 0];
      acc += 0. * gray[y + 0][x + 1];
      acc += 1. / 8. * gray[y + 0][x + 2];
      acc += -2. / 8. * gray[y + 1][x + 0];
      acc += 0. * gray[y + 1][x + 1];
      acc += 2. / 8. * gray[y + 1][x + 2];
      acc += -1. / 8. * gray[y + 2][x + 0];
      acc += 0. * gray[y + 2][x + 1];
      acc += 1. / 8. * gray[y + 2][x + 2];
      ix[y][x] = acc;
      float acc = 0.;
      acc += -1. / 8. * gray[y + 0][x + 0];
      acc += -2. / 8. * gray[y + 0][x + 1];
      acc += -1. / 8. * gray[y + 0][x + 2];
      acc += 0. / 8. * gray[y + 1][x + 0];
      acc += 0. / 8. * gray[y + 1][x + 1];
      acc += 0. / 8. * gray[y + 1][x + 2];
      acc += 1. / 8. * gray[y + 2][x + 0];
      acc += 2. / 8. * gray[y + 2][x + 1];
      acc += 1. / 8. * gray[y + 2][x + 2];
      iy[y][x] = acc;
    }
  }
  for (int y = 0; y < h - 2; y++) {
    for (int x = 0; x < w - 2; x++) {
      ixx[y][x] = ix[y][x] * ix[y][x];
      ixy[y][x] = ix[y][x] * iy[y][x];
      iyy[y][x] = iy[y][x] * iy[y][x];
    }
  }
  for (int y = 0; y < h - 2 - 3 + 1; y++) {
    for (int x = 0; x < w - 2 - 3 + 1; x++) {
      float acc = 0.;
      acc += 1. / 16. * ixx[y + 0][x + 0];
      acc += 2. / 16. * ixx[y + 0][x + 1];
      acc += 1. / 16. * ixx[y + 0][x + 2];
      acc += 2. / 16. * ixx[y + 1][x + 0];
      acc += 4. / 16. * ixx[y + 1][x + 1];
      acc += 2. / 16. * ixx[y + 1][x + 2];
      acc += 1. / 16. * ixx[y + 2][x + 0];
      acc += 2. / 16. * ixx[y + 2][x + 1];
      acc += 1. / 16. * ixx[y + 2][x + 2];
      sxx[y][x] = acc;
      float acc = 0.;
      acc += 1. / 16. * ixy[y + 0][x + 0];
      acc += 2. / 16. * ixy[y + 0][x + 1];
      acc += 1. / 16. * ixy[y + 0][x + 2];
      acc += 2. / 16. * ixy[y + 1][x + 0];
      acc += 4. / 16. * ixy[y + 1][x + 1];
      acc += 2. / 16. * ixy[y + 1][x + 2];
      acc += 1. / 16. * ixy[y + 2][x + 0];
      acc += 2. / 16. * ixy[y + 2][x + 1];
      acc += 1. / 16. * ixy[y + 2][x + 2];
      sxy[y][x] = acc;
      float acc = 0.;
      acc += 1. / 16. * iyy[y + 0][x + 0];
      acc += 2. / 16. * iyy[y + 0][x + 1];
      acc += 1. / 16. * iyy[y + 0][x + 2];
      acc += 2. / 16. * iyy[y + 1][x + 0];
      acc += 4. / 16. * iyy[y + 1][x + 1];
      acc += 2. / 16. * iyy[y + 1][x + 2];
      acc += 1. / 16. * iyy[y + 2][x + 0];
      acc += 2. / 16. * iyy[y + 2][x + 1];
      acc += 1. / 16. * iyy[y + 2][x + 2];
      syy[y][x] = acc;
    }
  }
  for (int y = 0; y < h - 2 - 2; y++) {
    for (int x = 0; x < w - 2 - 2; x++) {
      float det = sxx[y][x] * syy[y][x] - sxy[y][x] * sxy[y][x];
      float trace = sxx[y][x] + syy[y][x];
      out[y][x] = det - kappa * trace * trace;
    }
  }
  free(ix);
  free(iy);
  free(ixx);
  free(ixy);
  free(iyy);
  free(sxx);
  free(sxy);
  free(syy);
}
