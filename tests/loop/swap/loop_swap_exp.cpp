#include <optitrust.h>

void g(int* t) {
  __modifies("t ~> Matrix3(7, 10, 20)");
  __ghost(rewrite,
          "H1 := t ~> Matrix3(7, 10, 20), H2 := Group(range(0, 10, 1), fun b "
          "-> Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c -> "
          "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell))), by := swap_groups");
  for (int b = 0; b < 10; b++) {
    __modifies(
        "Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c -> "
        "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell))");
    __ghost(rewrite,
            "H1 := Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c "
            "-> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), H2 := "
            "Group(range(0, 20, 1), fun c -> Group(range(0, 7, 1), fun a -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), by := swap_groups");
    __ghost(rewrite,
            "H1 := Group(range(0, 20, 1), fun c -> Group(range(0, 7, 1), fun a "
            "-> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), H2 := "
            "Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), by := swap_groups");
    for (int a = 0; a < 7; a++) {
      __modifies(
          "Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, 10, 20, a, b, c)] ~> "
          "Cell)");
      for (int c = 0; c < 20; c++) {
        __modifies("&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell");
        t[MINDEX3(7, 10, 20, a, b, c)] = 0;
      }
    }
    __ghost(rewrite,
            "H1 := Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c "
            "-> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), H2 := "
            "Group(range(0, 20, 1), fun c -> Group(range(0, 7, 1), fun a -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), by := swap_groups");
    __ghost(rewrite,
            "H1 := Group(range(0, 20, 1), fun c -> Group(range(0, 7, 1), fun a "
            "-> &t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), H2 := "
            "Group(range(0, 7, 1), fun a -> Group(range(0, 20, 1), fun c -> "
            "&t[MINDEX3(7, 10, 20, a, b, c)] ~> Cell)), by := swap_groups");
  }
  __ghost(rewrite,
          "H1 := Group(range(0, 10, 1), fun b -> Group(range(0, 7, 1), fun a "
          "-> Group(range(0, 20, 1), fun c -> &t[MINDEX3(7, 10, 20, a, b, c)] "
          "~> Cell))), H2 := t ~> Matrix3(7, 10, 20), by := swap_groups");
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
  __ghost(rewrite,
          "H1 := v ~> Matrix2(n, m), H2 := Group(range(0, m, 1), fun y -> "
          "Group(range(0, n, 1), fun x -> &v[MINDEX2(n, m, x, y)] ~> Cell)), "
          "by := swap_groups");
  for (int y = 0; y < m; y++) {
    __sequentially_modifies("t ~> Matrix1(n)");
    __sequentially_reads("u ~> Matrix1(n)");
    __modifies(
        "Group(range(0, n, 1), fun x -> &v[MINDEX2(n, m, x, y)] ~> Cell)");
    for (int x = 0; x < n; x++) {
      __modifies("&t[MINDEX1(n, x)] ~> Cell");
      __modifies("&v[MINDEX2(n, m, x, y)] ~> Cell");
      __reads("&u[MINDEX1(n, x)] ~> Cell");
      t[MINDEX1(n, x)] = y * u[MINDEX1(n, x)];
      v[MINDEX2(n, m, x, y)] = t[MINDEX1(n, x)];
    }
  }
  __ghost(rewrite,
          "H1 := Group(range(0, m, 1), fun y -> Group(range(0, n, 1), fun x -> "
          "&v[MINDEX2(n, m, x, y)] ~> Cell)), H2 := v ~> Matrix2(n, m), by := "
          "swap_groups");
}

void seq_reads() {
  __pure();
  int x = 0;
  const __ghost_fn __ghost_pair_1 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  for (int j = 0; j < 5; j++) {
    __sequentially_reads("Group(range(0, 5, 1), fun i -> &x ~> Cell)");
    for (int i = 0; i < 5; i++) {
      __reads("&x ~> Cell");
      x + 1;
    }
  }
  __ghost_end(__ghost_pair_1);
}

void ghost_pairs() {
  __pure();
  int x = 0;
  const __ghost_fn __ghost_pair_3 =
      __ghost_begin(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
  const __ghost_fn __ghost_pair_2 = __ghost_begin(
      __with_reverse(
          [&]() {
            __requires("#126: _Fraction");
            __consumes("_RO(#126, Group(range(0, 5, 1), fun i -> &x ~> Cell))");
            __produces(
                "_RO(#126 / range_count(range(0, 5, 1)), Group(range(0, 5, 1), "
                "fun i -> Group(range(0, 5, 1), fun _ -> &x ~> Cell)))");
            for (int i = 0; i < 5; i++) {
              __loop_ghosts("#126: _Fraction");
              __consumes("_RO(#126, &x ~> Cell)");
              __produces(
                  "_RO(#126 / range_count(range(0, 5, 1)), Group(range(0, 5, "
                  "1), fun _ -> &x ~> Cell))");
              __ghost(ro_fork_group, "H := &x ~> Cell, r := range(0, 5, 1)");
            }
          },
          [&]() {
            for (int i = 0; i < 5; i++) {
              __loop_ghosts("#126: _Fraction");
              __consumes(
                  "_RO(#126 / range_count(range(0, 5, 1)), Group(range(0, 5, "
                  "1), fun j -> &x ~> Cell))");
              __produces("_RO(#126, &x ~> Cell)");
              __ghost(ro_join_group, "H := &x ~> Cell, r := range(0, 5, 1)");
            }
          }),
      "");
  for (int k = 0; k < 5; k++) {
    __sequentially_reads(
        "Group(range(0, 5, 1), fun i -> Group(range(0, 5, 1), fun j -> &x ~> "
        "Cell))");
    for (int i = 0; i < 5; i++) {
      __reads("Group(range(0, 5, 1), fun j -> &x ~> Cell)");
      for (int j = 0; j < 5; j++) {
        __reads("&x ~> Cell");
        x + 1;
      }
    }
  }
  __ghost_end(__ghost_pair_2);
  __ghost_end(__ghost_pair_3);
}
