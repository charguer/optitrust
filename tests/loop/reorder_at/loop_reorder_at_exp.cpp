#include <optitrust.h>

void f1(int* y) {
  __modifies("y ~> Matrix2(4, 4)");
  int x = 0;
  int z = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&z ~> Cell");
    __sequentially_modifies(
        "Group(range(0, 4, 1), fun b -> Group(range(0, 4, 1), fun c -> "
        "&y[MINDEX2(4, 4, b, c)] ~> Cell))");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell");
      x++;
      x++;
    }
    __ghost(swap_groups,
            "outer_range := range(0, 4, 1), inner_range := range(0, 4, 1), "
            "items := fun b, c -> &y[MINDEX2(4, 4, b, c)] ~> Cell");
    for (int c = 0; c < 4; c++) {
      __modifies(
          "Group(range(0, 4, 1), fun b -> &y[MINDEX2(4, 4, b, c)] ~> Cell)");
      for (int b = 0; b < 4; b++) {
        __modifies("&y[MINDEX2(4, 4, b, c)] ~> Cell");
        y[MINDEX2(4, 4, b, c)]++;
      }
    }
    __ghost(swap_groups,
            "outer_range := range(0, 4, 1), inner_range := range(0, 4, 1), "
            "items := fun c, b -> &y[MINDEX2(4, 4, b, c)] ~> Cell");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&z ~> Cell");
      z++;
      z++;
    }
  }
}

void f2(float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p)");
  __reads("B ~> Matrix2(p, n)");
  float* const sum = (float* const)MALLOC2(m, m, sizeof(float));
  for (int i = 0; i < m; i++) {
    __parallel_reads("A ~> Matrix2(m, p)");
    __parallel_reads("B ~> Matrix2(p, n)");
    __consumes(
        "_Uninit(Group(range(0, m, 1), fun _v1 -> &sum[MINDEX2(m, m, i, _v1)] "
        "~> Cell))");
    __produces(
        "Group(range(0, m, 1), fun j -> &sum[MINDEX2(m, m, i, j)] ~> Cell)");
    for (int j = 0; j < m; j++) {
      __consumes("_Uninit(&sum[MINDEX2(m, m, i, j)] ~> Cell)");
      __produces("&sum[MINDEX2(m, m, i, j)] ~> Cell");
      sum[MINDEX2(m, m, i, j)] = 0.f;
    }
  }
  const __ghost_fn __ghost_pair_8 = __ghost_begin(
      ro_fork_group, "H := A ~> Matrix2(m, p), r := range(0, m, 1)");
  const __ghost_fn __ghost_pair_7 = __ghost_begin(
      ro_fork_group, "H := B ~> Matrix2(p, n), r := range(0, m, 1)");
  const __ghost_fn __ghost_pair_4 = __ghost_begin(
      __with_reverse(
          [&]() {
            __requires("#_3: _Fraction");
            __consumes(
                "_RO(#_3, Group(range(0, m, 1), fun i -> B ~> Matrix2(p, n)))");
            __produces(
                "_RO(#_3 / range_count(range(0, m, 1)), Group(range(0, m, 1), "
                "fun i -> Group(range(0, m, 1), fun _ -> B ~> Matrix2(p, "
                "n))))");
            for (int i = 0; i < m; i++) {
              __loop_ghosts("#_4: _Fraction");
              __consumes("_RO(#_4, B ~> Matrix2(p, n))");
              __produces(
                  "_RO(#_4 / range_count(range(0, m, 1)), Group(range(0, m, "
                  "1), fun _ -> B ~> Matrix2(p, n)))");
              __ghost(ro_fork_group,
                      "H := B ~> Matrix2(p, n), r := range(0, m, 1)");
            }
          },
          [&]() {
            for (int i = 0; i < m; i++) {
              __loop_ghosts("#_3: _Fraction");
              __consumes(
                  "_RO(#_3 / range_count(range(0, m, 1)), Group(range(0, m, "
                  "1), fun j -> B ~> Matrix2(p, n)))");
              __produces("_RO(#_3, B ~> Matrix2(p, n))");
              __ghost(ro_join_group,
                      "H := B ~> Matrix2(p, n), r := range(0, m, 1)");
            }
          }),
      "");
  const __ghost_fn __ghost_pair_3 = __ghost_begin(
      __with_reverse(
          [&]() {
            __requires("#_3: _Fraction");
            __consumes(
                "_RO(#_3, Group(range(0, m, 1), fun i -> A ~> Matrix2(m, p)))");
            __produces(
                "_RO(#_3 / range_count(range(0, m, 1)), Group(range(0, m, 1), "
                "fun i -> Group(range(0, m, 1), fun _ -> A ~> Matrix2(m, "
                "p))))");
            for (int i = 0; i < m; i++) {
              __loop_ghosts("#_4: _Fraction");
              __consumes("_RO(#_4, A ~> Matrix2(m, p))");
              __produces(
                  "_RO(#_4 / range_count(range(0, m, 1)), Group(range(0, m, "
                  "1), fun _ -> A ~> Matrix2(m, p)))");
              __ghost(ro_fork_group,
                      "H := A ~> Matrix2(m, p), r := range(0, m, 1)");
            }
          },
          [&]() {
            for (int i = 0; i < m; i++) {
              __loop_ghosts("#_3: _Fraction");
              __consumes(
                  "_RO(#_3 / range_count(range(0, m, 1)), Group(range(0, m, "
                  "1), fun j -> A ~> Matrix2(m, p)))");
              __produces("_RO(#_3, A ~> Matrix2(m, p))");
              __ghost(ro_join_group,
                      "H := A ~> Matrix2(m, p), r := range(0, m, 1)");
            }
          }),
      "");
  for (int k = 0; k < p; k++) {
    __sequentially_modifies("sum ~> Matrix2(m, m)");
    __parallel_reads(
        "Group(range(0, m, 1), fun i -> Group(range(0, m, 1), fun j -> A ~> "
        "Matrix2(m, p)))");
    __parallel_reads(
        "Group(range(0, m, 1), fun i -> Group(range(0, m, 1), fun j -> B ~> "
        "Matrix2(p, n)))");
    for (int i = 0; i < m; i++) {
      __modifies(
          "Group(range(0, m, 1), fun j -> &sum[MINDEX2(m, m, i, j)] ~> Cell)");
      __reads("Group(range(0, m, 1), fun j -> A ~> Matrix2(m, p))");
      __reads("Group(range(0, m, 1), fun j -> B ~> Matrix2(p, n))");
      for (int j = 0; j < m; j++) {
        __modifies("&sum[MINDEX2(m, m, i, j)] ~> Cell");
        __reads("A ~> Matrix2(m, p)");
        __reads("B ~> Matrix2(p, n)");
        __ghost(matrix2_ro_focus, "M := A, i := i, j := k");
        __ghost(matrix2_ro_focus, "M := B, i := k, j := j");
        sum[MINDEX2(m, m, i, j)] +=
            A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "M := A");
        __ghost(matrix2_ro_unfocus, "M := B");
      }
    }
  }
  __ghost_end(__ghost_pair_3);
  __ghost_end(__ghost_pair_4);
  __ghost_end(__ghost_pair_7);
  __ghost_end(__ghost_pair_8);
  for (int i = 0; i < m; i++) {
    __parallel_reads("A ~> Matrix2(m, p)");
    __parallel_reads("B ~> Matrix2(p, n)");
    __consumes(
        "Group(range(0, m, 1), fun j -> &sum[MINDEX2(m, m, i, j)] ~> Cell)");
    __produces(
        "_Uninit(Group(range(0, m, 1), fun _v1 -> &sum[MINDEX2(m, m, i, _v1)] "
        "~> Cell))");
    for (int j = 0; j < m; j++) {
      __consumes("&sum[MINDEX2(m, m, i, j)] ~> Cell");
      __produces("_Uninit(&sum[MINDEX2(m, m, i, j)] ~> Cell)");
      sum[MINDEX2(m, m, i, j)]++;
    }
  }
  MFREE2(m, m, sum);
}

void f1_wrong() {
  __pure();
  int x = 0;
  int y = 0;
  for (int a = 0; a < 4; a++) {
    __sequentially_modifies("&x ~> Cell");
    __sequentially_modifies("&y ~> Cell");
    for (int b = 0; b < 4; b++) {
      __sequentially_modifies("&x ~> Cell");
      __sequentially_modifies("&y ~> Cell");
      x = 0;
      for (int c = 0; c < 4; c++) {
        x += c;
      }
      y += x;
    }
  }
}
