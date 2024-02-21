#include <optitrust.h>

void demo_both_par(int* t, int n, int m) {
  __modifies(
      "Group(range(0, n, 1), fun i -> Group(range(0, m, 1), fun j -> &t[i * m "
      "+ j] ~> Cell))");
  __ghost(swap_groups,
          "outer_range := range(0, n, 1), inner_range := range(0, m, 1), items "
          ":= fun i, j -> &t[i * m + j] ~> Cell");
  for (int j = 0; j < m; j++) {
    __modifies("Group(range(0, n, 1), fun i -> &t[i * m + j] ~> Cell)");
    for (int i = 0; i < n; i++) {
      __modifies("&t[i * m + j] ~> Cell");
      t[i * m + j] = j;
    }
  }
  __ghost(swap_groups,
          "outer_range := range(0, m, 1), inner_range := range(0, n, 1), items "
          ":= fun j, i -> &t[i * m + j] ~> Cell");
}

void demo_outer_par(int* t, int n) {
  __modifies("Group(range(0, n, 1), fun i -> &t[i] ~> Cell)");
  for (int j = 0; j < 4; j++) {
    __sequentially_modifies("Group(range(0, n, 1), fun i -> &t[i] ~> Cell)");
    for (int i = 0; i < n; i++) {
      __modifies("&t[i] ~> Cell");
      t[i] = j;
    }
  }
}

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");
  __ghost(swap_groups,
          "outer_range := range(0, 7, 1), inner_range := range(0, 10, 1), "
          "items := fun a, b -> Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, "
          "10, 20, a, b, c)] ~> Cell)");
  for (int b = 0; b < 10; b++) {
    __modifies(
        "Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c -> "
        "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell))");
    __ghost(swap_groups,
            "outer_range := range(0, 7, 1), inner_range := range(0, 20, 1), "
            "items := fun a, c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    __ghost(swap_groups,
            "outer_range := range(0, 20, 1), inner_range := range(0, 7, 1), "
            "items := fun c, a -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    for (int a = 0; a < 7; a++) {
      __modifies(
          "Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> "
          "Cell)");
      for (int c = 0; c < 20; c++) {
        __modifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
    __ghost(swap_groups,
            "outer_range := range(0, 7, 1), inner_range := range(0, 20, 1), "
            "items := fun a, c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
    __ghost(swap_groups,
            "outer_range := range(0, 20, 1), inner_range := range(0, 7, 1), "
            "items := fun c, a -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
  }
  __ghost(swap_groups,
          "outer_range := range(0, 10, 1), inner_range := range(0, 7, 1), "
          "items := fun b, a -> Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, "
          "10, 20, a, b, c)] ~> Cell)");
  for (int i = 0; i < 10; i++) {
    __pure();
    for (int j = i; j < i + 1; j++) {
      __pure();
    }
  }
}

void f(int* t, int* u, int* v, int n, int m) {
  __modifies("t ~> Matrix1(n)");
  __modifies("v ~> Matrix2(n, m)");
  __reads("u ~> Matrix1(n)");
  __ghost(swap_groups,
          "outer_range := range(0, n, 1), inner_range := range(0, m, 1), items "
          ":= fun x, y -> &v[MINDEX2(n, m, x, y)] ~> Cell");
  for (int y = 0; y < m; y++) {
    __sequentially_modifies("t ~> Matrix1(n)");
    __parallel_reads("u ~> Matrix1(n)");
    __modifies(
        "Group(range(0, n, 1), fun x -> &v[MINDEX2(n, m, x, y)] ~> Cell)");
    for (int x = 0; x < n; x++) {
      __modifies("&v[MINDEX2(n, m, x, y)] ~> Cell");
      __modifies("&t[MINDEX1(n, x)] ~> Cell");
      __reads("&u[MINDEX1(n, x)] ~> Cell");
      t[MINDEX1(n, x)] = y * u[MINDEX1(n, x)];
      v[MINDEX2(n, m, x, y)] = t[MINDEX1(n, x)];
    }
  }
  __ghost(swap_groups,
          "outer_range := range(0, m, 1), inner_range := range(0, n, 1), items "
          ":= fun y, x -> &v[MINDEX2(n, m, x, y)] ~> Cell");
}

void par_reads() {
  __pure();
  int x = 0;
  for (int j = 0; j < 5; j++) {
    __parallel_reads("&x ~> Cell");
    for (int i = 0; i < 5; i++) {
      __parallel_reads("&x ~> Cell");
      x + 1;
    }
  }
}

void ghost_pairs(int* x) {
  __reads("x ~> Matrix1(1)");
  for (int i = 0; i < 5; i++) {
    __parallel_reads("x ~> Matrix1(1)");
    const __ghost_fn focus_x = __ghost_begin(matrix1_ro_focus, "i := 0");
    for (int j = 0; j < 5; j++) {
      __parallel_reads("&x[MINDEX1(1, 0)] ~> Cell");
      x[MINDEX1(1, 0)] + 1;
    }
    __ghost_end(focus_x);
  }
}
