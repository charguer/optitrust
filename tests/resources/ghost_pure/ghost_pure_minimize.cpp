#include <optitrust.h>

__GHOST(trivial_init) {
  __requires("k: int");
  __ensures("Triv(k)");
  __admitted();
}

__GHOST(trivial_change) {
  __requires("k: int, old_k: int, Triv(old_k)");
  __ensures("Triv(k)");
  __admitted();
}

void req_triv(int k) {
  __requires("Triv(k)");
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

void g() {
  __pure();
  for (int i = 0; i < 100; ++i) {
    __ensures("Triv(i)");
    __ghost(trivial_init, "i");
    __ghost(trivial_init, "i*2");
    __ghost(trivial_init, "i+12");
    __ghost(trivial_change, "i*3, i*2");
    req_triv(i+12);
  }
}
