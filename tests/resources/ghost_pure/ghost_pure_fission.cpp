#include <optitrust.h>

__DECL(Triv, "int -> Prop");

__GHOST(trivial_init) {
  __requires("k: int");
  __ensures("triv: Triv(k)");
  __admitted();
}

__GHOST(trivial_change) {
  __requires("k: int, old_k: int, old_triv: Triv(old_k)");
  __ensures("triv: Triv(k)");
  __admitted();
}

void req_triv(int k) {
  __requires("triv: Triv(k)");
}

void f() {
  __pure();

  const int k = 0;
  k+1;
  __ghost(trivial_init, "k");
  k+2;
  __ghost(trivial_change, "k+3");
  __ghost(trivial_change, "k+4");
  req_triv(k+3);
  req_triv(k+4);
  __ghost(trivial_change, "k+5");
}

void req_refl(int k) {
  __requires("refl: k = k");
}

void let_ghost(int k) {
  __pure();

  __ASSERT(refl_k, "k = k");
  req_refl(k); __with("refl_k");
  req_refl(k); __with("refl_k");
}

void with_clear(int k) {
  __ensures("k = k");

  __ASSERT(refl_k1, "k = k");
  __clear("refl_k1");
  __ASSERT(refl_k2, "k = k");
  __ASSERT(refl_k3, "k = k");
}
