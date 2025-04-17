#include <optitrust_models.h>

void f() {
  __pure();

  // FIXME how not const ?
  float* const M = MALLOC1(float, 10);
  free(M);
}
