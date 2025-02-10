#include <optitrust.h>

void f() {
  __pure();
  float* const M = (float*)calloc(MSIZE1(32), sizeof(float));
  __ghost(matrix1_focus, "M := M, i := 0");
  M[MINDEX1(32, 0)] = 0.f;
  __ghost(matrix1_unfocus, "M := M");
  free(M);
}

void g(float* t) {
  __modifies("&t[0] ~> Cell");
  t[0] = 0.f;
}
