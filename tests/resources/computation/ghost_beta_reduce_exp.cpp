#include <optitrust.h>

__ghost_ret test() {
  __requires("f: int -> Prop");
  __requires("k: int");
  __requires("valid: f(k)");
  __ensures("f(k + 1)");
  __admitted();
}

__ghost_ret two_closures() {
  __requires("f: int -> Prop -> Prop");
  __requires("g: int -> Prop");
  __ensures("f(g)");
  __admitted();
}

void f() {
  __requires("P: int -> Prop");
  __requires("P(0)");
  __ghost(test, "f := fun i -> P(i), k := 0");
  __ghost(test, "f := fun i -> P(i + 1), k := 0");
  __ghost(two_closures, "f := fun g -> g(1), g := fun x -> P(x - 1)");
}

void g(float* M) {
  __modifies("M ~> Matrix1(1024)");
  __ghost(tile_divides, "tile_size := 256, tile_count := 4");
  __ghost(untile_divides,
          "size := 1024, items := fun i -> &M[MINDEX1(1024, i)] ~> Cell");
}

void g2(float* M) {
  __modifies("M ~> Matrix1(1024)");
  const __ghost_fn tileM =
      __ghost_begin(tile_divides, "tile_size := 256, tile_count := 4");
  __ghost_end(tileM);
}
