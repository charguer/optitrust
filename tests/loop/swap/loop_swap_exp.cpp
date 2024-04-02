#include <optitrust.h>

void demo_both_par(int* t, int n, int m) {
  __modifies("for i in 0..n -> for j in 0..m -> &t[i * m + j] ~> Cell");
  __ghost(swap_groups,
          "outer_range := 0..n, inner_range := 0..m, items := fun i, j -> &t[i "
          "* m + j] ~> Cell");
  for (int j = 0; j < m; j++) {
    __strict();
    __modifies("for i in 0..n -> &t[i * m + j] ~> Cell");
    for (int i = 0; i < n; i++) {
      __strict();
      __modifies("&t[i * m + j] ~> Cell");
      t[i * m + j] = j;
    }
  }
  __ghost(swap_groups,
          "outer_range := 0..m, inner_range := 0..n, items := fun j, i -> &t[i "
          "* m + j] ~> Cell");
}

void demo_outer_par(int* t, int n) {
  __modifies("for i in 0..n -> &t[i] ~> Cell");
  for (int j = 0; j < 4; j++) {
    __strict();
    __sequentially_modifies("for i in 0..n -> &t[i] ~> Cell");
    for (int i = 0; i < n; i++) {
      __strict();
      __modifies("&t[i] ~> Cell");
      t[i] = j;
    }
  }
}

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");
  __ghost(swap_groups,
          "outer_range := 0..7, inner_range := 0..10, items := fun a, b -> for "
          "c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
  for (int b = 0; b < 10; b++) {
    __strict();
    __modifies(
        "for a in 0..7 -> for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> "
        "Cell");
    __ghost(swap_groups,
            "outer_range := 0..7, inner_range := 0..20, items := fun a, c -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    __ghost(swap_groups,
            "outer_range := 0..20, inner_range := 0..7, items := fun c, a -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    for (int a = 0; a < 7; a++) {
      __strict();
      __modifies("for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
      for (int c = 0; c < 20; c++) {
        __strict();
        __modifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
    __ghost(swap_groups,
            "outer_range := 0..7, inner_range := 0..20, items := fun a, c -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    __ghost(swap_groups,
            "outer_range := 0..20, inner_range := 0..7, items := fun c, a -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
  }
  __ghost(swap_groups,
          "outer_range := 0..10, inner_range := 0..7, items := fun b, a -> for "
          "c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
  for (int i = 0; i < 10; i++) {
    __strict();
    for (int j = i; j < i + 1; j++) {
      __strict();
    }
  }
}

void f(int* t, int* u, int* v, int n, int m) {
  __modifies("t ~> Matrix1(n)");
  __modifies("v ~> Matrix2(n, m)");
  __reads("u ~> Matrix1(n)");
  __ghost(swap_groups,
          "outer_range := 0..n, inner_range := 0..m, items := fun x, y -> "
          "&v[MINDEX2(n, m, x, y)] ~> Cell");
  for (int y = 0; y < m; y++) {
    __strict();
    __sequentially_modifies("t ~> Matrix1(n)");
    __parallel_reads("u ~> Matrix1(n)");
    __modifies("for x in 0..n -> &v[MINDEX2(n, m, x, y)] ~> Cell");
    for (int x = 0; x < n; x++) {
      __strict();
      __modifies("&v[MINDEX2(n, m, x, y)] ~> Cell");
      __modifies("&t[MINDEX1(n, x)] ~> Cell");
      __reads("&u[MINDEX1(n, x)] ~> Cell");
      t[MINDEX1(n, x)] = y * u[MINDEX1(n, x)];
      v[MINDEX2(n, m, x, y)] = t[MINDEX1(n, x)];
    }
  }
  __ghost(swap_groups,
          "outer_range := 0..m, inner_range := 0..n, items := fun y, x -> "
          "&v[MINDEX2(n, m, x, y)] ~> Cell");
}

void par_reads() {
  __pure();
  int x = 0;
  for (int j = 0; j < 5; j++) {
    __strict();
    __parallel_reads("&x ~> Cell");
    for (int i = 0; i < 5; i++) {
      __strict();
      __parallel_reads("&x ~> Cell");
      x + 1;
    }
  }
}

void indep_reads(int* M) {
  __reads("M ~> Matrix2(5, 5)");
  for (int j = 0; j < 5; j++) {
    __strict();
    __parallel_reads("M ~> Matrix2(5, 5)");
    for (int i = 0; i < 5; i++) {
      __strict();
      __reads("for j in 0..5 -> &M[MINDEX2(5, 5, i, j)] ~> Cell");
      const __ghost_fn __ghost_pair_1 = __ghost_begin(
          group_ro_focus,
          "i := j, items := fun j -> &M[MINDEX2(5, 5, i, j)] ~> Cell");
      M[MINDEX2(5, 5, i, j)];
      __ghost_end(__ghost_pair_1);
    }
  }
}

void ghost_pairs(int* x) {
  __reads("x ~> Matrix1(1)");
  for (int i = 0; i < 5; i++) {
    __strict();
    __parallel_reads("x ~> Matrix1(1)");
    const __ghost_fn focus_x =
        __ghost_begin(matrix1_ro_focus, "M := x, i := 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __parallel_reads("&x[MINDEX1(1, 0)] ~> Cell");
      x[MINDEX1(1, 0)] + 1;
    }
    __ghost_end(focus_x);
  }
}

void ghost_pure(int* M) {
  __reads("M ~> Matrix1(1024)");
  for (int bi = 0; bi < 128; ++bi) {
    __strict();
    __parallel_reads("M ~> Matrix1(1024)");
    for (int j = 0; j < 4; ++j) {
      __strict();
      __parallel_reads("M ~> Matrix1(1024)");
      for (int i = 0; i < 8; ++i) {
        __strict();
        __parallel_reads("M ~> Matrix1(1024)");
        __ghost(tiled_index_in_range,
                "tile_index := bi, index := i, tile_count := 128, tile_size := "
                "8, size := 1024");
        const __ghost_fn focus =
            __ghost_begin(matrix1_ro_focus, "M := M, i := bi * 8 + i");
        M[MINDEX1(1024, bi * 8 + i)];
        __ghost_end(focus);
      }
    }
  }
}
