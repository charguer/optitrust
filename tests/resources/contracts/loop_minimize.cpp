#include <optitrust.h>

void unused_modifies(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n), M2 ~> Matrix1(n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __modifies("&M1[MINDEX1(n, i)] ~> Cell, &M2[MINDEX1(n, i)] ~> Cell");

    c += M1[MINDEX1(n, i)];
  }
}

void unused_reads(float* M1, float* M2, int n) {
  __reads("M1 ~> Matrix1(n), M2 ~> Matrix1(n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __reads("&M1[MINDEX1(n, i)] ~> Cell, &M2[MINDEX1(n, i)] ~> Cell");

    c += M1[MINDEX1(n, i)];
  }
}

void produced_uninit_used_ro(int* t2) {
  __consumes("t2 ~> Matrix1(10)");
  __produces("_Uninit(t2 ~> Matrix1(10))");

  for (int i = 0; i < 10; i++) {
    __consumes("&t2[MINDEX1(10, i)] ~> Cell");
    __produces("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");

    int x = t2[MINDEX1(10, i)];
  }

  for (int i = 0; i < 10; i++) {
    __consumes("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    __produces("&t2[MINDEX1(10, i)] ~> Cell");

    t2[MINDEX1(10, i)] = 2;
  }

  for (int i = 0; i < 10; i++) {
    __consumes("&t2[MINDEX1(10, i)] ~> Cell");
    __produces("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");

    t2[MINDEX1(10, i)] = 2;
  }
}

void nested_loops(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix2(n, n), M2 ~> Matrix2(n, n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __modifies("for j in 0..n -> &M1[MINDEX2(n, n, i, j)] ~> Cell");
    __modifies("for j in 0..n -> &M2[MINDEX2(n, n, i, j)] ~> Cell");

    int acc = 0;
    for (int j = 0; j < n; j++) {
      __sequentially_modifies("&c ~> Cell, &acc ~> Cell");
      __modifies("&M1[MINDEX2(n, n, i, j)] ~> Cell, &M2[MINDEX2(n, n, i, j)] ~> Cell");

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
    __sequentially_modifies("&x ~> Cell, &acc ~> Cell");
    acc += x;
  }
}

__GHOST(assert_in_range) {
  __requires("i: int, n: int, in_range(i, 0..n)");
}

void useless_pure_facts(int n, int i) {
  __requires("in_range(i, 0..n), 0 <= i");

  for (int j = 0; j < 100; j++) {
    __loop_ghosts("k: int");
    __invariant("in_range(k, 0..n), 0 <= k, 1 = 1");
    __ghost(assert_in_range, "k, n");
  }
}
