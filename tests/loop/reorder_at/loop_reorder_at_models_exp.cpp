#include <optitrust_models.h>

__ghost(assert_inhabited, "x := arbitrary(int)", "unknown <- x");

__ghost_ret intro_unknown() {
  __requires("p: ptr(int)");
  __requires("v: int");
  __consumes("p ~~> v");
  __produces("p ~~> unknown");
  __admitted();
}

__ghost_ret elim_unknown() {
  __requires("p: ptr(int)");
  __consumes("p ~~> unknown");
  __ensures("v: int");
  __produces("p ~~> v");
  __admitted();
}

void f1(int* y) {
  __requires("Y: int * int -> int");
  __consumes("y ~> Matrix2(4, 4, Y)");
  __produces("y ~> Matrix2(4, 4, fun (i: int) (j: int) -> Y(i, j) + 4)");
  int x = 0;
  int z = 0;
  __ghost(intro_unknown, "p := &x");
  __ghost(intro_unknown, "p := &z");
  for (int i = 0; i < 4; i++) {
    __strict();
    __xconsumes("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j)");
    __xproduces("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + 0");
    for (int j = 0; j < 4; j++) {
      __strict();
      __xconsumes("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j)");
      __xproduces("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + 0");
      __ghost(rewrite_linear,
              "inside := fun v -> &y[MINDEX2(4, 4, i, j)] ~~> v, by := "
              "plus_zero_intro(Y(i, j))");
    }
  }
  for (int a = 0; a < 4; a++) {
    __strict();
    __spreserves("&x ~~> unknown");
    __spreserves("&z ~~> unknown");
    __spreserves(
        "for b in 0..4 -> for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) "
        "+ a");
    for (int b = 0; b < 4; b++) {
      __strict();
      __spreserves("&x ~~> unknown");
      __ghost(elim_unknown, "p := &x");
      x++;
      x++;
      __ghost(intro_unknown, "p := &x");
    }
    __ghost(swap_groups,
            "outer_range := 0..4, inner_range := 0..4, items := fun (b: int) "
            "(c: int) -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");
    for (int c = 0; c < 4; c++) {
      __strict();
      __xconsumes("for b in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");
      __xproduces(
          "for b in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a + 1");
      for (int b = 0; b < 4; b++) {
        __strict();
        __xconsumes("&y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");
        __xproduces("&y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a + 1");
        y[MINDEX2(4, 4, b, c)]++;
      }
    }
    __ghost(swap_groups,
            "outer_range := 0..4, inner_range := 0..4, items := fun (c: int) "
            "(b: int) -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a + 1");
    for (int b = 0; b < 4; b++) {
      __strict();
      __spreserves("&z ~~> unknown");
      __ghost(elim_unknown, "p := &z");
      z++;
      z++;
      __ghost(intro_unknown, "p := &z");
    }
    for (int i = 0; i < 4; i++) {
      __strict();
      __xconsumes(
          "for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + a + 1");
      __xproduces(
          "for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + (a + 1)");
      for (int j = 0; j < 4; j++) {
        __strict();
        __xconsumes("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + a + 1");
        __xproduces("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + (a + 1)");
        __ghost(rewrite_linear,
                "inside := fun v -> &y[MINDEX2(4, 4, i, j)] ~~> v, by := "
                "add_assoc_right(Y(i, j), a, 1)");
      }
    }
  }
}

void f2(int* A, int* B, int m, int n, int p) {
  __requires("a: int * int -> int");
  __requires("b: int * int -> int");
  __reads("A ~> Matrix2(m, p, a)");
  __reads("B ~> Matrix2(p, n, b)");
  int* const sum = (int*)malloc(MSIZE2(m, n) * sizeof(int));
  for (int i = 0; i < m; i++) {
    __strict();
    __xwrites("for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~~> unknown");
    for (int j = 0; j < n; j++) {
      __strict();
      __xwrites("&sum[MINDEX2(m, n, i, j)] ~~> unknown");
      sum[MINDEX2(m, n, i, j)] = 0;
      __ghost(intro_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
    }
  }
  for (int k = 0; k < p; k++) {
    __strict();
    __spreserves(
        "for i in 0..m -> for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~~> "
        "unknown");
    __sreads("A ~> Matrix2(m, p, a)");
    __sreads("B ~> Matrix2(p, n, b)");
    for (int i = 0; i < m; i++) {
      __strict();
      __sreads("A ~> Matrix2(m, p, a)");
      __sreads("B ~> Matrix2(p, n, b)");
      __xpreserves("for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~~> unknown");
      for (int j = 0; j < n; j++) {
        __strict();
        __sreads("A ~> Matrix2(m, p, a)");
        __sreads("B ~> Matrix2(p, n, b)");
        __xpreserves("&sum[MINDEX2(m, n, i, j)] ~~> unknown");
        __ghost(ro_matrix2_focus, "matrix := A, i := i, j := k");
        __ghost(ro_matrix2_focus, "matrix := B, i := k, j := j");
        __ghost(elim_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
        sum[MINDEX2(m, n, i, j)] +=
            A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(intro_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
        __ghost(ro_matrix2_unfocus, "matrix := A");
        __ghost(ro_matrix2_unfocus, "matrix := B");
      }
    }
  }
  for (int i = 0; i < m; i++) {
    __strict();
    __xconsumes("for j in 0..n -> &sum[MINDEX2(m, n, i, j)] ~~> unknown");
    __xproduces("for _v1 in 0..n -> &sum[MINDEX2(m, n, i, _v1)] ~> UninitCell");
    for (int j = 0; j < n; j++) {
      __strict();
      __xconsumes("&sum[MINDEX2(m, n, i, j)] ~~> unknown");
      __xproduces("&sum[MINDEX2(m, n, i, j)] ~> UninitCell");
      __ghost(elim_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
      sum[MINDEX2(m, n, i, j)]++;
      __ghost(intro_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
      __ghost(elim_unknown, "p := &sum[MINDEX2(m, n, i, j)]");
    }
  }
  free(sum);
}
