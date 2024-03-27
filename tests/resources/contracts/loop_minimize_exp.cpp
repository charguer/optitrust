#include <optitrust.h>

void unused_modifies(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n)");
  __modifies("M2 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __sequentially_modifies("&c ~> Cell");
    __reads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void unused_reads(float* M1, float* M2, int n) {
  __reads("M1 ~> Matrix1(n)");
  __reads("M2 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __sequentially_modifies("&c ~> Cell");
    __reads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void produced_uninit_used_ro(int* t2) {
  __consumes("t2 ~> Matrix1(10)");
  __produces("_Uninit(t2 ~> Matrix1(10))");
  for (int i = 0; i < 10; i++) {
    __strict();
    __reads("&t2[MINDEX1(10, i)] ~> Cell");
    int x = t2[MINDEX1(10, i)];
  }
  for (int i = 0; i < 10; i++) {
    __strict();
    __writes("&t2[MINDEX1(10, i)] ~> Cell");
    t2[MINDEX1(10, i)] = 2;
  }
  for (int i = 0; i < 10; i++) {
    __strict();
    __modifies("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    t2[MINDEX1(10, i)] = 2;
  }
}

void nested_loops(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix2(n, n)");
  __modifies("M2 ~> Matrix2(n, n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __sequentially_modifies("&c ~> Cell");
    __reads("for j in 0..n -> &M1[MINDEX2(n, n, i, j)] ~> Cell");
    int acc = 0;
    for (int j = 0; j < n; j++) {
      __strict();
      __sequentially_modifies("&acc ~> Cell");
      __reads("&M1[MINDEX2(n, n, i, j)] ~> Cell");
      acc += M1[MINDEX2(n, n, i, j)];
    }
    c += acc;
  }
}

void seq_modifies_into_par_reads() {
  __pure();
  int x = 1;
  int acc = 0;
  for (int i = 0; i < 100; i++) {
    __strict();
    __sequentially_modifies("&acc ~> Cell");
    __parallel_reads("&x ~> Cell");
    acc += x;
  }
}

__ghost_ret assert_in_range() {
  __requires("i: int");
  __requires("n: int");
  __requires("in_range(i, 0..n)");
}

void useless_pure_facts(int n, int i) {
  __requires("in_range(i, 0..n)");
  __requires("__is_leq(0, i)");
  for (int j = 0; j < 100; j++) {
    __strict();
    __loop_ghosts("k: int");
    __invariant("in_range(k, 0..n)");
    __ghost(assert_in_range, "i := k, n := n");
  }
}

void useless_exclusive_pure_facts(int n, int i) {
  __requires("in_range(i, 0..n)");
  for (int k = 0; k < 10; k++) {
    __strict();
    __ensures("in_range(i, 0..n + 3)");
    __ensures("in_range(i, 0..n)");
    __ghost(in_range_extend, "x := i, r1 := 0..n, r2 := 0..n + 3");
  }
  for (int k = 0; k < 10; k++) {
    __strict();
    __requires("in_range(i, 0..n + 3)");
    for (int j = 0; j < 100; j++) {
      __strict();
      __ghost(assert_in_range, "i := i, n := n + 3");
    }
  }
}
