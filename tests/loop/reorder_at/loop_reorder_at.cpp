#include <optitrust_models.h>

__DECL(unknown, "int");

__GHOST(intro_unknown) {
  __requires("p: ptr(int), v: int");
  __consumes("p ~~> v");
  __produces("p ~~> unknown");
  __admitted();
}

__GHOST(elim_unknown) {
  __requires("p: ptr(int)");
  __ensures("v: int");
  __consumes("p ~~> unknown");
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
    __xconsumes("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j)");
    __xproduces("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + 0");
    for (int j = 0; j < 4; j++) {
      __xconsumes("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j)");
      __xproduces("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + 0");
      __ghost(rewrite_linear, "inside := fun v -> &y[MINDEX2(4, 4, i, j)] ~~> v, by := plus_zero_intro(Y(i, j))");
    }
  }
  for (int a = 0; a < 4; a++) {
    __strict();
    __spreserves("&x ~~> unknown, &z ~~> unknown");
    __spreserves("for b in 0..4 -> for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");

    for (int b = 0; b < 4; b++) {
      __strict();
      __spreserves("&x ~~> unknown, &z ~~> unknown");
      __xconsumes("for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");
      __xproduces("for c in 0..4 -> &y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a + 1");

      __ghost(elim_unknown, "p := &x");
      x++;
      x++;
      __ghost(intro_unknown, "p := &x");
      for (int c = 0; c < 4; c++) {
        __strict();
        __xconsumes("&y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a");
        __xproduces("&y[MINDEX2(4, 4, b, c)] ~~> Y(b, c) + a + 1");
        y[MINDEX2(4, 4, b, c)]++;
      }
      __ghost(elim_unknown, "p := &z");
      z++;
      z++;
      __ghost(intro_unknown, "p := &z");
    }
    for (int i = 0; i < 4; i++) {
      __xconsumes("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + a + 1");
      __xproduces("for j in 0..4 -> &y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + (a + 1)");
      for (int j = 0; j < 4; j++) {
        __xconsumes("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + a + 1");
        __xproduces("&y[MINDEX2(4, 4, i, j)] ~~> Y(i, j) + (a + 1)");
        __ghost(rewrite_linear, "inside := fun v -> &y[MINDEX2(4, 4, i, j)] ~~> v, by := add_assoc_right(Y(i, j), a, 1)");
      }
    }
  }
}

void f2(int* A, int* B, int m, int n, int p) {
  __requires("a: int * int -> int");
  __requires("b: int * int -> int");
  __reads("A ~> Matrix2(m, p, a), B ~> Matrix2(p, n, b)");

  for (int i = 0; i < m; i++) {
    __strict();
    __sreads("A ~> Matrix2(m, p, a), B ~> Matrix2(p, n, b)");

    for (int j = 0; j < n; j++) {
      __strict();
      __sreads("A ~> Matrix2(m, p, a), B ~> Matrix2(p, n, b)");

      int sum = 0;
      __ghost(intro_unknown, "p := &sum");
      for (int k = 0; k < p; k++) {
        __strict();
        __sreads("A ~> Matrix2(m, p, a), B ~> Matrix2(p, n, b)");
        __smodifies("&sum ~~> unknown");

        __ghost(ro_matrix2_focus, "A, i, k");
        __ghost(ro_matrix2_focus, "B, k, j");
        __ghost(elim_unknown, "p := &sum");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(intro_unknown, "p := &sum");
        __ghost(ro_matrix2_unfocus, "A");
        __ghost(ro_matrix2_unfocus, "B");
      }
      __ghost(elim_unknown, "p := &sum");
      sum++;
      __ghost(intro_unknown, "p := &sum");
      __ghost(elim_unknown, "p := &sum");
    }
  }
}
/*
void f1_wrong() {
  __pure();

  int x = 0;
  int y = 0;
  for (int a = 0; a < 4; a++) {
    __strict();
    __smodifies("&x ~> Cell, &y ~> Cell");

    for (int b = 0; b < 4; b++) {
      __strict();
      __smodifies("&x ~> Cell, &y ~> Cell");

      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
*/
