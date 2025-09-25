#include <optitrust.h>

#include "box_blur.h"
#include "omp.h"

void blur(float* output, float* input, int w, int h) {
  const int w2 = w - 8;
  const int h2 = h - 2;
  float* blur_y = (float*)malloc(MSIZE2(h2, w) * sizeof(float));
#pragma omp parallel for
  for (int by = 0; by < h2; by += 32) {
    for (int y = by; y < by + 32 && y < h2; y++) {
      for (int bx = 0; bx < exact_div(w, 4); bx++) {
#pragma omp simd
        for (int x = 0; x < 4; x++) {
          blur_y[bx * 4 + x + w * y] = (input[bx * 4 + x + w * (y + 0)] +
                                        input[bx * 4 + x + w * (y + 1)] +
                                        input[bx * 4 + x + w * (y + 2)]) /
                                       3.f;
        }
      }
      for (int bx = 0; bx < exact_div(w2, 4); bx++) {
#pragma omp simd
        for (int x = 0; x < 4; x++) {
          output[bx * 4 + x + w2 * y] =
              (blur_y[bx * 4 + x + 0 + w * y] + blur_y[bx * 4 + x + 1 + w * y] +
               blur_y[bx * 4 + x + 2 + w * y]) /
              3.f;
        }
      }
    }
  }
}
