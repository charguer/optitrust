#include <optitrust.h>

__GHOST(trivial_init) {
  __requires("k: int");
  __ensures("k = k");
  __admitted();
}

__GHOST(trivial_change) {
  __requires("k: int, old_k: int, old_k = old_k");
  __ensures("k = k");
  __admitted();
}

void req_triv(int k) {
  __requires("k = k");
}

void f() {
  __pure();

  const int k = 0;
  k+1;
  __ghost(trivial_init, "k");
  k+2;
  __ghost(trivial_change, "k+3");
  __ghost(trivial_change, "k+4");
  for (int i = 0; i < 10; ++i) {
    req_triv(k+3);
    req_triv(k+4);
  }
  __ghost(trivial_change, "k+5");
}
