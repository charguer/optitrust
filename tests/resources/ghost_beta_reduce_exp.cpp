#include "../../include/optitrust.h"

__ghost_ret test() {
  __requires("f: _Fun(int, formula), k: int, valid: f(k)");
  __ensures("f(k + 1)");
  __admitted();
}

__ghost_ret two_closures() {
  __requires("f: _Fun(_Fun(formula, formula), formula), g: _Fun(int, formula)");
  __ensures("f(g)");
  __admitted();
}

void f() {
  __pure();
  __ghost(test, "f := fun i -> P(i), k := 0, valid := checked");
  __ghost(test, "f := fun i -> P(i + 1), k := 0");
  __ghost(two_closures, "f := fun g -> g(1), g := fun x -> P(x - 1)");
}

void g(float* M) {
  __modifies("M ~> Matrix2(1024, 1024)");
  __ghost(tile_divides,
          "tile_size := 256, tile_count := 4, wand_id := 0, bound_check := "
          "checked");
  __ghost(close_wand, "wand_id := 0");
}
