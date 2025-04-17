#include <optitrust_models.h>

void f() {
  __pure();
  float* const M = (float*)malloc(MSIZE1(10) * sizeof(float));
  free(M);
}
