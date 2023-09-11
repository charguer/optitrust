#include "../../include/optitrust.h"

int div_exact(int a, int b) {
  __requires("q: int; proof: __assert_eq(a, b * q);");
  __ensures("__assert_eq(_Res, q);");
  __admitted();
  return a / b;
}

void any_proof() { __requires("prop: formula; proof: prop;"); }

void caller() {
  __pure();
  int y = __ghost_args(div_exact(6, 3), "q := 2, proof := checked");
}
