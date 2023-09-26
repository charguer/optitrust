#include "../../include/optitrust.h"

float* array_alloc(int len) {
  __produces("_Res ~> Array(len)");
  __admitted();
  return (float*)malloc(len * sizeof(float));
}

void array_free(float* M) {
  __requires("dim: int");
  __consumes("M ~> Array(dim)");
  __admitted();
  free(M);
}

__ghost_ret array_focus() {
  __requires("M: ptr, i: int, dim: int");
  __consumes("M ~> Array(dim)");
  __produces("&M[i] ~> Cell, M ~> FocussedArray(dim, i)");
  __admitted();
}

__ghost_ret array_unfocus() {
  __requires("M: ptr, i: int, dim: int");
  __consumes("M ~> FocussedArray(dim, i), &M[i] ~> Cell");
  __produces("M ~> Array(dim)");
  __admitted();
}

void f() {
  __pure();
  float* const M = array_alloc(32);
  __ghost(array_focus, "M := M, i := 0");
  M[0] = 0;
  __ghost(array_unfocus, "M := M");
  array_free(M);
}

void g(float* t) {
  __modifies("&t[0] ~> Cell");
  t[0] = 0;
}
