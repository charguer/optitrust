#include <optitrust.h>

void pair(int* M) {
  __reads("M ~> Matrix1(10)");
  __GHOST_BEGIN(f, matrix1_ro_focus, "M, 2");
  int k1 = M[MINDEX1(10, 2)];
  int k2 = M[MINDEX1(10, 2)];
  int k3 = M[MINDEX1(10, 2)];
  int k4 = M[MINDEX1(10, 2)];
  __GHOST_END(f);
}

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

void pure_facts() {
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
  k+5;
  k+6;
  const int z = 0;
  req_triv(k+5);
}

void pure_noop() {
  __pure();

  __ghost(trivial_init, "0");
  __ghost(trivial_change, "1");
  req_triv(0);
}
