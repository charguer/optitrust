#include <optitrust.h>

void unused_modifies(float* M1, float* M2, int n) {
  __modifies("M1 ~> Matrix1(n), M2 ~> Matrix1(n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&c ~> Cell");
    __xreads("&M1[MINDEX1(n, i)] ~> Cell");

    c += M1[MINDEX1(n, i)];
  }
}

void unused_reads(float* M1, float* M2, int n) {
  __reads("M1 ~> Matrix1(n), M2 ~> Matrix1(n)");

  int c = 0;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&c ~> Cell");
    __xreads("&M1[MINDEX1(n, i)] ~> Cell");

    c += M1[MINDEX1(n, i)];
  }
}

void produced_uninit_used_ro(int* t2) {
  __consumes("t2 ~> Matrix1(10)");
  __produces("_Uninit(t2 ~> Matrix1(10))");

  for (int i = 0; i < 10; i++) {
    __strict();
    __xreads("&t2[MINDEX1(10, i)] ~> Cell");

    int x = t2[MINDEX1(10, i)];
  }
}

__GHOST(assert_in_range) {
  __requires("i: int, n: int, in_range(i, 0..n)");
}

void useless_pure_facts(int n, int i) {
  __requires("in_range(i, 0..n), 0 <= i");
  __ghost(assert_in_range, "i, n");
}

void merge_frac(int* M, int n) {
  __requires("f: _Frac");
  __consumes("_RO(1-f, M ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");
  __produces("M ~> Matrix2(n, n)");

  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f");
}

void multi_merge_frac(int* M, int n) {
  __requires("f1: _Frac, f2: _Frac, f3: _Frac");
  __consumes(
    "_RO(1-f1-f2-f3, M ~> Matrix2(n, n)),"
    "_RO(f1, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell),"
    "_RO(f3, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");
  __produces("_RO(1-f2, M ~> Matrix2(n, n))");

  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f1");
  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f3");
}

void read_and_merge_frac(int* M, int n) {
  __requires("f: _Frac");
  __consumes("_RO(1-f, M ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");
  __produces("M ~> Matrix2(n, n)");

  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __xreads("for j in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell");
    for (int j = 0; j < n; ++j) {
      __xreads("&M[MINDEX2(n,n,i,j)] ~> Cell");
      acc += M[MINDEX2(n,n,i,j)];
    }
  }
  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f");
}

void read_and_multi_merge_frac(int* M, int n) {
  __requires("f1: _Frac, f2: _Frac, f3: _Frac");
  __consumes(
    "_RO(1-f1-f2-f3, M ~> Matrix2(n, n)),"
    "_RO(f1, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell),"
    "_RO(f3, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");
  __produces("_RO(1-f2, M ~> Matrix2(n, n))");

  int acc = 0;
  for (int i = 0; i < n; ++i) {
    __xreads("for j in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell");
    for (int j = 0; j < n; ++j) {
      __xreads("&M[MINDEX2(n,n,i,j)] ~> Cell");
      acc += M[MINDEX2(n,n,i,j)];
    }
  }
  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f1");
  __ghost(ro_swap_groups, "items := fun j, i -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := f3");
}

// Typing of these functions is broken
void split_frac_generic(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __ensures("f: _Frac");
  __produces("_RO(1-f, M ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
}

void split_subfrac_generic(int* M, int n) {
  __requires("a: _Frac");
  __consumes("_RO(a, M ~> Matrix2(n, n))");
  __ensures("f: _Frac");
  __produces("_RO(a-f, M ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
}

/*void split_frac_generic_opposite(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __ensures("f: _Frac");
  __produces("_RO(f, M ~> Matrix2(n, n)), _RO(1-f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
}*/

void split_frac_generic_lossy(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __ensures("f: _Frac, g: _Frac");
  __produces("_RO(g, M ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
}

/*void split_frac_same(int* M1, int* M2, int n) {
  __consumes("M1 ~> Matrix2(n, n)");
  __consumes("M2 ~> Matrix2(n, n)");
  __ensures("f: _Frac");
  __produces("_RO(1-f, M1 ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M1[MINDEX2(n,n,i,j)] ~> Cell)");
  __produces("_RO(1-f, M2 ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M2[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M1[MINDEX2(n,n,i,j)] ~> Cell");
  __ghost(ro_swap_groups, "items := fun i, j -> &M2[MINDEX2(n,n,i,j)] ~> Cell");
}

void split_frac_twice(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __ensures("f: _Frac");
  __produces("_RO(1-f-f, M1 ~> Matrix2(n, n)), _RO(f, for j in 0..n -> for i in 0..n -> &M1[MINDEX2(n,n,i,j)] ~> Cell), _RO(f, for j in 0..n -> for i in 0..n -> &M1[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell");
}*/

void split_frac_specific(int* M, int n) {
  __consumes("M ~> Matrix2(n, n)");
  __produces("_RO(1-1/2, M ~> Matrix2(n, n)), _RO(1/2, for j in 0..n -> for i in 0..n -> &M[MINDEX2(n,n,i,j)] ~> Cell)");

  __ghost(ro_split2, "f := 1, H := M ~> Matrix2(n,n)");
  __ghost(ro_swap_groups, "items := fun i, j -> &M[MINDEX2(n,n,i,j)] ~> Cell, f := 1/2");
  __ghost(ro_allow_join2, "f := 1, H := M ~> Matrix2(n,n)");
}
