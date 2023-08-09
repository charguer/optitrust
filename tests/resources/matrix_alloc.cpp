#include "../../include/optitrust.h"

void f() {
  __pure();

  // FIXME how not const ?
  float* const M = (float*)MALLOC1(10, sizeof(float));
  MFREE1(10, M);
}