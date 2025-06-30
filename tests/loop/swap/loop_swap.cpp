#include <optitrust.h>

void demo_both_par(int* t, int n, int m) {
  __modifies("for i in 0..n -> for j in 0..m ->"
             " &t[(i * m + j)] ~> Cell");
  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("for j in 0..m -> &t[(i * m + j)] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __xmodifies("&t[(i * m + j)] ~> Cell");
      t[i * m + j] = j;
    }
  }
}

void demo_outer_par(int* t, int n) {
  __modifies("for i in 0..n -> &t[i] ~> Cell");

  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("&t[i] ~> Cell");

    for (int j = 0; j < 4; j++) {
      __strict();
      __smodifies("&t[i] ~> Cell");
      t[i] = j;
    }
  }
}

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");

  for (int a = 0 ; a < 7; a++) {
    __xmodifies("for b in 0..10 ->"
               "for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    for(int b = 0; b < 10; b++) {
      __xmodifies("for c in 0..20 -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
      for(int c = 0; c < 20; c++){
        __xmodifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
  }

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

  for (int x = 0 ; x < n; x++) {
    __strict();
    __xmodifies("&t[MINDEX1(n,x)] ~> Cell");
    __xmodifies("for y in 0..m -> &v[MINDEX2(n,m,x,y)] ~> Cell");
    __xreads("&u[MINDEX1(n,x)] ~> Cell");

    for (int y = 0; y < m; y++) {
      __strict();
      __xmodifies("&v[MINDEX2(n,m,x,y)] ~> Cell");
      __smodifies("&t[MINDEX1(n,x)] ~> Cell");
      __sreads("&u[MINDEX1(n,x)] ~> Cell");

      t[MINDEX1(n,x)] = y * u[MINDEX1(n,x)];
      v[MINDEX2(n,m,x,y)] = t[MINDEX1(n,x)];
    }
  }
}

void par_reads() {
  __pure();

  int x = 0;
  for (int i = 0; i < 5; i++) {
    __strict();
    __sreads("&x ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __sreads("&x ~> Cell");
      x + 1;
    }
  }
}

void indep_reads(int* M) {
  __reads("M ~> Matrix2(5, 5)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __xreads("for j in 0..5 -> &M[MINDEX2(5,5,i,j)] ~> Cell");
    for (int j = 0; j < 5; j++) {
      __strict();
      __xreads("&M[MINDEX2(5,5,i,j)] ~> Cell");
      M[MINDEX2(5,5,i,j)];
    }
  }
}

void ghost_pairs(int* x) {
  __reads("x ~> Matrix1(1)");

  for (int i = 0; i < 5; i++) {
    __strict();
    __sreads("x ~> Matrix1(1)");
    __GHOST_BEGIN(focus_x, ro_matrix1_focus, "x, 0");
    for (int j = 0; j < 5; j++) {
      __strict();
      __sreads("&x[MINDEX1(1,0)] ~> Cell");
      x[MINDEX1(1,0)] + 1;
    }
    __GHOST_END(focus_x);
  }
}

void ghost_pure(int* M) {
  __reads("M ~> Matrix1(1024)");

  for (int bi = 0; bi < 128; ++bi) {
    for (int i = 0; i < 8; ++i) {
      __ghost(tiled_index_in_range, "bi, i, 128, 8, 1024");
      for (int j = 0; j < 4; ++j) {
        __GHOST_BEGIN(focus, ro_matrix1_focus, "M, bi * 8 + i");
        M[MINDEX1(1024, bi * 8 + i)];
        __GHOST_END(focus);
      }
    }
  }
}

void ghost_linear_pure(Args) {
  contracts ?
      for (int i = 0; i < 4; i++) {
        __strict();
        __sreads("S ~> Matrix2(1024, 1024)");
        __xmodifies(
            "for j in 0..1024 -> &D[MINDEX2(1024, 1024, bi * 4 + i, j)] ~> "
            "Cell");
        __ghost(tiled_index_in_range,
                "tile_index := bi, index := i, div_check := tile_div_check_i");
        __ghost(assert_prop, "P := __is_true(1024 == 256 * 4)",
                "tile_div_check_j <- proof");
        __ghost(tile_divides,
                "div_check := tile_div_check_j, items := fun (j: int) -> "
                "&D[MINDEX2(1024, 1024, bi * 4 + i, j)] ~> Cell");
        __ghost(
            color,
            "nb_colors := 2, size := 256, items := fun (bj: int) -> for j in "
            "0..4 -> &D[MINDEX2(1024, 1024, bi * 4 + i, bj * 4 + j)] ~> Cell");
        for (int cj = 0; cj < 2; cj++) {
          __strict();
          __sreads("S ~> Matrix2(1024, 1024)");
          __xmodifies(
              "for bj in range(cj, 256, 2) -> for j in 0..4 -> "
              "&D[MINDEX2(1024, 1024, bi * 4 + i, bj * 4 + j)] ~> Cell");
          __admitted();
        }
        __ghost(
            uncolor,
            "nb_colors := 2, size := 256, items := fun (bj: int) -> for j in "
            "0..4 -> &D[MINDEX2(1024, 1024, bi * 4 + i, bj * 4 + j)] ~> Cell");
        __ghost(untile_divides,
                "div_check := tile_div_check_j, items := fun (j: int) -> "
                "&D[MINDEX2(1024, 1024, bi * 4 + i, j)] ~> Cell");
      }
}
