#include <optitrust_models.h>

#include "omp.h"

float dot(float* a, float* b, int n) {
  float s = 0.f;
  float* const t = (float*)malloc(exact_div(n, 32) * sizeof(float));
#pragma omp parallel for
  for (int bi = 0; bi < exact_div(n, 32); bi++) {
    t[bi] = ({
      float arith_res = 0;
      const float arith_res1 = arith_res;
      arith_res1;
    });
    for (int i = 0; i < 32; i++) {
      t[bi] = ({
        float arith_res = a[32 * bi + i] * b[32 * bi + i] + t[bi];
        const float arith_res2 = arith_res;
        arith_res2;
      });
    }
  }
  for (int bi = 0; bi < exact_div(n, 32); bi++) {
    s = ({
      float get = ({
        float arith_res = t[bi] + s;
        const float arith_res3 = arith_res;
        arith_res3;
      });
      const float getc = get;
      getc;
    });
  }
  free(t);
  return s;
}
