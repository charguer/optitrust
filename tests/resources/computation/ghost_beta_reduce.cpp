#include <optitrust.h>

__GHOST(test) {
  __requires("f: int -> formula, k: int, valid: f(k)");
  __ensures("f(k+1)");
  __admitted();
}

__GHOST(two_closures) {
  __requires("f: (formula -> formula) -> formula, g: int -> formula");
  __ensures("f(g)");
  __admitted();
}

void f() {
  __pure();
  __ghost(test, "f := fun i -> P(i), k := 0, valid := checked");
  __ghost(test, "f := fun i -> P(i+1), k := 0");
  __ghost(two_closures, "f := fun g -> g(1), g := fun x -> P(x-1)");
}

void g(float* M) {
  __modifies("M ~> Matrix1(1024)");
  __ghost(tile_divides, "tile_size := 256, tile_count := 4, bound_check := checked");
  __ghost(untile_divides, "n := 1024, items := fun i -> &M[MINDEX1(1024, i)] ~> Cell, bound_check := checked");
}

void g2(float* M) {
  __modifies("M ~> Matrix1(1024)");
  __GHOST_BEGIN(tileM, tile_divides, "tile_size := 256, tile_count := 4, bound_check := checked");
  __GHOST_END(tileM);
}
