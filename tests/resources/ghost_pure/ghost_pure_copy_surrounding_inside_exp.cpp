#include <optitrust.h>

__ghost_ret trivial_init() {
  __requires("k: int");
  __ensures("__is_true(k == k)");
  __admitted();
}

__ghost_ret trivial_change() {
  __requires("k: int");
  __requires("old_k: int");
  __requires("__is_true(old_k == old_k)");
  __ensures("__is_true(k == k)");
  __admitted();
}

void req_triv(int k) { __requires("__is_true(k == k)"); }

void f() {
  __pure();
  const int k = 0;
  k + 1;
  k + 2;
  for (int i = 0; i < 10; ++i) {
    __strict();
    __ghost(trivial_init, "k := k");
    __ghost(trivial_change, "k := k + 3");
    __ghost(trivial_change, "k := k + 4");
    req_triv(k + 3);
    req_triv(k + 4);
  }
}
