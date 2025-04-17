#include <optitrust_models.h>

__GHOST(test) {
  __requires("f: int -> Prop, k: int, valid: f(k)");
  __ensures("f(k+1)");
  __admitted();
}

__GHOST(two_closures) {
  __requires("f: (int -> Prop) -> Prop, g: int -> Prop");
  __ensures("f(g)");
  __admitted();
}

void f() {
  __requires("P: int -> Prop");
  __requires("P(0)");
  __ghost(test, "f := fun i -> P(i), k := 0");
  __ghost(test, "f := fun i -> P(i+1), k := 0");
  __ghost(two_closures, "f := fun g -> g(1), g := fun x -> P(x-1)");
}

void g(float* M) {
  __requires("model: int -> float");
  __preserves("M ~> Matrix1(1024, model)");
  __ghost(tile_divides, "tile_size := 256, tile_count := 4");
  __ghost(untile_divides, "size := 1024, items := fun i -> &M[MINDEX1(1024, i)] ~~> model(i)");
}

void g2(float* M) {
  __requires("model: int -> float");
  __preserves("M ~> Matrix1(1024, model)");
  __GHOST_BEGIN(tileM, tile_divides, "tile_size := 256, tile_count := 4");
  __GHOST_END(tileM);
}
