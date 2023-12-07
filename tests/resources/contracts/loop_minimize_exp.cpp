#include <optitrust.h>

void default_contract() {
  __pure();
  int x = 0;
  int y = 0;
  int z = 0;
  for (int i = 0; i < 3; ++i) {
    __sequentially_modifies("&z ~> Cell");
    __sequentially_reads("&y ~> Cell");
    z = y;
  }
}

void iter_contract(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n)");
  __modifies("M2 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __reads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void produced_uninit_used_ro(int* t2) {
  __consumes("t2 ~> Matrix1(10)");
  __produces("_Uninit(t2 ~> Matrix1(10))");
  for (int i = 0; i < 10; i++) {
    __reads("&t2[MINDEX1(10, i)] ~> Cell");
    int x = t2[MINDEX1(10, i)];
  }
  for (int i = 0; i < 10; i++) {
    __consumes("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    __produces("&t2[MINDEX1(10, i)] ~> Cell");
    t2[MINDEX1(10, i)] = 2;
  }
  for (int i = 0; i < 10; i++) {
    __modifies("_Uninit(&t2[MINDEX1(10, i)] ~> Cell)");
    t2[MINDEX1(10, i)] = 2;
  }
}

void nested_loops(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix2(n, n)");
  __modifies("M2 ~> Matrix2(n, n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&c ~> Cell");
    __reads("Group(range(0, n, 1), fun j -> &M1[MINDEX2(n, n, i, j)] ~> Cell)");
    int acc = 0;
    for (int j = 0; j < n; j++) {
      __sequentially_modifies("&acc ~> Cell");
      __reads("&M1[MINDEX2(n, n, i, j)] ~> Cell");
      acc += M1[MINDEX2(n, n, i, j)];
    }
    c += acc;
  }
}
