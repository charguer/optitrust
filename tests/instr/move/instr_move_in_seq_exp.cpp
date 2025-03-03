#include <optitrust.h>

void pair(int* M) {
  __reads("M ~> Matrix1(10)");
  const __ghost_fn __ghost_pair_4 =
      __ghost_begin(matrix1_ro_focus, "M := M, i := 2");
  int k2 = M[MINDEX1(10, 2)];
  int k3 = M[MINDEX1(10, 2)];
  __ghost_end(__ghost_pair_4);
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(matrix1_ro_focus, "M := M, i := 2");
  int k4 = M[MINDEX1(10, 2)];
  __ghost_end(__ghost_pair_1);
  const __ghost_fn __ghost_pair_6 =
      __ghost_begin(matrix1_ro_focus, "M := M, i := 2");
  int k1 = M[MINDEX1(10, 2)];
  __ghost_end(__ghost_pair_6);
}

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

void pure_facts() {
  __pure();
  const int k = 0;
  __ghost(trivial_init, "k := k");
  __ghost(trivial_change, "k := k + 3");
  req_triv(k + 3);
  __ghost(trivial_change, "k := k + 4");
  req_triv(k + 4);
  k + 5;
  k + 6;
  const int z = 0;
  __ghost(trivial_init, "k := k");
  __ghost(trivial_change, "k := k + 3");
  __ghost(trivial_change, "k := k + 4");
  __ghost(trivial_change, "k := k + 5");
  req_triv(k + 5);
  k + 1;
  k + 2;
  /*@ m__begin, m__end @*/
}

void pure_noop() {
  __pure();
  __ghost(trivial_init, "k := 0");
  req_triv(0);
}

void with_assert_alias(int x) {
  __pure();
  if (x == 0) {
    __ghost(assert_alias, "x := x, y := 0");
    __ghost(assert_alias, "x := x, y := 0");
    int b = x + 2;
    __ghost(assert_alias, "x := x, y := 0");
    int a = x + 1;
  }
}
