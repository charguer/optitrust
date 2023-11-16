#include "../../../include/optitrust.h"

void var(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(&x ~> Cell)");
    __modifies("&t[MINDEX1(3, i)] ~> Cell");
    x = 3;
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong1(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(&x ~> Cell)");
    __modifies("&t[MINDEX1(3, i)] ~> Cell");
    x = i; // can't move out, depends on 'i'
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong2(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(&x ~> Cell)");
    __modifies("&t[MINDEX1(3, i)] ~> Cell");
    t[MINDEX1(3, i)] = 0;
    x = 3; // can't move out, not first in sequence
    t[MINDEX1(3, i)] += x;
  }
}

void var_wrong3(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("&x ~> Cell");
    __modifies("&t[MINDEX1(3, i)] ~> Cell");
    x += 3; // can't move out, reads previous 'x'
    t[MINDEX1(3, i)] = x;
  }
}

void var_wrong4(int* t) {
  __modifies("t ~> Matrix1(3)");

  int x = 0;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(&x ~> Cell)");
    __modifies("&t[MINDEX1(3, i)] ~> Cell");

    x = 3; // can't move out, previous iterations modify 'x'
    x++;
    t[MINDEX1(3, i)] = x;
  }
}

void var_needs_if(int* t, int n) {
  __modifies("t ~> Matrix1(n)");

  int x = 0;
  for (int i = 0; i < n; i++) {
    __sequentially_modifies("&x ~> Cell");
    __modifies("&t[MINDEX1(n, i)] ~> Cell");

    x = 3;
    t[MINDEX1(n, i)] = x;
  }
  // can't move out x = 3 without an additional if
  // if the range is empty, x should be 0 here.
  int y = x;
}

void arr(int* t, int* x) {
  __modifies("t ~> Matrix2(3, 5), _Uninit(x ~> Matrix1(5))");

  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(x ~> Matrix1(5))");
    __modifies("Group(range(0, 5, 1), fun j -> &t[MINDEX2(3, 5, i, j)] ~> Cell)");

    for (int j = 0; j < 5; j++) {
      __writes("&x[MINDEX1(5, j)] ~> Cell");
      x[MINDEX1(5, j)] = 3;
    }
    for (int j = 0; j < 5; j++) {
      __writes("&t[MINDEX2(3, 5, i, j)] ~> Cell");
      __reads("&x[MINDEX1(5, j)] ~> Cell");
      t[MINDEX2(3, 5, i, j)] = x[MINDEX1(5, j)];
    }
  }
}

void arr_wrong1(int* t, int* x) {
  __modifies("t ~> Matrix2(3, 5), _Uninit(x ~> Matrix1(5))");

  int v = 3;
  for (int i = 0; i < 3; i++) {
    __sequentially_modifies("_Uninit(x ~> Matrix1(5)), &v ~> Cell");
    __modifies("Group(range(0, 5, 1), fun j -> &t[MINDEX2(3, 5, i, j)] ~> Cell)");

    for (int j = 0; j < 5; j++) { // can't move out, 'v' is not loop invariant
      __sequentially_reads("&v ~> Cell");
      __writes("&x[MINDEX1(5, j)] ~> Cell");
      x[MINDEX1(5, j)] = v;
    }
    for (int j = 0; j < 5; j++) {
      __writes("&t[MINDEX2(3, 5, i, j)] ~> Cell");
      __reads("&x[MINDEX1(5, j)] ~> Cell");
      t[MINDEX2(3, 5, i, j)] = x[MINDEX1(5, j)];
    }
    v++;
  }
}

void test(int* t){
  __modifies("_Uninit(t ~> Matrix1(10))");

  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    __sequentially_reads("&a ~> Cell, &b ~> Cell");
    __modifies("_Uninit(&t[MINDEX1(10, i)] ~> Cell)");

    int r = i;
    for (int j = 0; j < 10; j++) {
      __sequentially_reads("&a ~> Cell, &b ~> Cell");
      __sequentially_modifies("_Uninit(&t[MINDEX1(10, i)] ~> Cell)");

      int s = i;
      int x = a + b;
      t[MINDEX1(10, i)] = i;
    }
  }
}
