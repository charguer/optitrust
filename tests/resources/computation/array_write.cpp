#include <optitrust.h>

void f() {
  __pure();

  float* const M = CALLOC1(float, 32);
  __ghost(matrix1_focus, "M, 0");
  M[MINDEX1(32,0)] = 0.f;
  __ghost(matrix1_unfocus, "M");
  free(M);
}

void g(float* t) {
  __modifies("&t[0] ~> Cell");
  t[0] = 0.f;
}
