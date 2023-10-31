#include "../../../include/optitrust.h"

int div_exact(int a, int b) {
  __requires("q: int");
  __requires("proof: __assert_eq(a, b * q)");
  __ensures("__assert_eq(_Res, q)");
  __admitted();
  return a / b;
}

void f() { __requires("k: int"); }

int g(int x) {
  __pure();
  __admitted();
  return x;
}

__ghost_ret any_proof() {
  __requires("prop: formula");
  __requires("proof: prop");
}

void caller() {
  __pure();
  int y = div_exact(6, 3);
  __with("q := 2, proof := checked");
  f();
  __with("k := 6");
  __ghost(any_proof, "prop := A, proof := a");
  int z = g(__call_with(div_exact(12, 2), "q := 6, proof := checked"));
}
