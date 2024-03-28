#include <optitrust.h>

__ghost_ret trivial_init() {
  __requires("k: int");
  __ensures("Triv(k)");
  __admitted();
}

__ghost_ret trivial_change() {
  __requires("k: int");
  __requires("old_k: int");
  __requires("Triv(old_k)");
  __ensures("Triv(k)");
  __admitted();
}

void req_triv(int k) { __requires("Triv(k)"); }

void f() {
  __pure();
  const int k = 0;
  /*@ m0 @*/
  k + 1;
  /*@ m1, m2 @*/
  k + 2;
  /*@ m3, m4, m5, fission @*/
  __ghost(trivial_init, "k := k");
  __ghost(trivial_change, "k := k + 3");
  req_triv(k + 3);
  /*@ m6, fission @*/
  __ghost(trivial_init, "k := k");
  __ghost(trivial_change, "k := k + 3");
  __ghost(trivial_change, "k := k + 4");
  req_triv(k + 4);
  /*@ m7 @*/
}
