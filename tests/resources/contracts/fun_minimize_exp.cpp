#include <optitrust.h>

void unused_modifies(float* M1, float* M2, int n) {
  __reads("M1 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&c ~> Cell");
    __xreads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void unused_reads(float* M1, float* M2, int n) {
  __reads("M1 ~> Matrix1(n)");
  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&c ~> Cell");
    __xreads("&M1[MINDEX1(n, i)] ~> Cell");
    c += M1[MINDEX1(n, i)];
  }
}

void produced_uninit_used_ro(int* t2) {
  __reads("t2 ~> Matrix1(10)");
  for (int i = 0; i < 10; i++) {
    __strict();
    __xreads("&t2[MINDEX1(10, i)] ~> Cell");
    int x = t2[MINDEX1(10, i)];
  }
}

__ghost_ret assert_in_range() {
  __requires("i: int");
  __requires("n: int");
  __requires("in_range(i, 0..n)");
}

void useless_pure_facts(int n, int i) {
  __requires("in_range(i, 0..n)");
  __ghost(assert_in_range, "i := i, n := n");
}

void merge_frac(int* M, int n) {
  __requires("f: _Frac");
  __consumes(
      "_RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __produces("_RO(f, M ~> Matrix2(n, n))");
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f");
}

void multi_merge_frac(int* M, int n) {
  __requires("f1: _Frac");
  __requires("f3: _Frac");
  __consumes(
      "_RO(f1, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __consumes(
      "_RO(f3, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __produces("_RO(f3, M ~> Matrix2(n, n))");
  __produces("_RO(f1, M ~> Matrix2(n, n))");
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f1");
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f3");
}

void read_and_merge_frac(int* M, int n) {
  __requires("f: _Frac");
  __consumes(
      "_RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __produces("_RO(f, M ~> Matrix2(n, n))");
  __reads("M ~> Matrix2(n, n)");
  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __strict();
    __smodifies("&acc ~> Cell");
    __xreads("for j in 0..n -> &M[MINDEX2(n, n, i, j)] ~> Cell");
    for (int j = 0; j < n; ++j) {
      __strict();
      __smodifies("&acc ~> Cell");
      __xreads("&M[MINDEX2(n, n, i, j)] ~> Cell");
      acc += M[MINDEX2(n, n, i, j)];
    }
  }
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f");
}

void read_and_multi_merge_frac(int* M, int n) {
  __requires("f1: _Frac");
  __requires("f3: _Frac");
  __consumes(
      "_RO(f1, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __consumes(
      "_RO(f3, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __produces("_RO(f3, M ~> Matrix2(n, n))");
  __produces("_RO(f1, M ~> Matrix2(n, n))");
  __reads("M ~> Matrix2(n, n)");
  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __strict();
    __smodifies("&acc ~> Cell");
    __xreads("for j in 0..n -> &M[MINDEX2(n, n, i, j)] ~> Cell");
    for (int j = 0; j < n; ++j) {
      __strict();
      __smodifies("&acc ~> Cell");
      __xreads("&M[MINDEX2(n, n, i, j)] ~> Cell");
      acc += M[MINDEX2(n, n, i, j)];
    }
  }
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f1");
  __ghost(ro_swap_groups,
          "items := fun j, i -> &M[MINDEX2(n, n, i, j)] ~> Cell, f := f3");
}

void split_frac_specific(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __produces("_RO(1 - 1 / 2, M ~> Matrix2(n, n))");
  __produces(
      "_RO(1 / 2, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n, n, i, j)] ~> "
      "Cell)");
  __ghost(ro_swap_groups,
          "items := fun i, j -> &M[MINDEX2(n, n, i, j)] ~> Cell");
}
