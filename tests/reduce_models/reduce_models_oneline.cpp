#include <optitrust_models.h>

__ghost_ret plus1() {
  __requires("n: int");
  __requires("np1: int");
  __requires("__is_true(np1 == n + 1)");
  __ensures("eq: __is_true(np1 == n + 1)");
}

void rowSumW3(int* s, int* d, int n, int cn) {
  __requires("S: int * int -> int");
  __requires("__is_true(n >= 1)");
  __requires("__is_true(cn >= 0)");

  const int w = 3;
  __ghost(assert_alias, "x := w, y := 3");
  __ASSERT(wpos, "__is_true(w >= 0)");

  __writes(
      "d ~> Matrix2(n, cn, fun (i: int) (c: int) -> reduce_int_sum(i, i + w, "
      "fun k -> S(k, c)))");
  __reads("s ~> Matrix2(n + w - 1, cn, S)");

    __ghost([&]() {
      __requires("__is_true(w >= 0)");
      __requires("__is_true(w == 3)");
      __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
      __ensures("__is_true(3 >= 0)");
      __ensures("__is_true(3 == 3)");
      __produces("_RO(#_4, s ~> Matrix2(n + 2, cn, S))");
      __admitted();
    });
    __ghost(assume, "P := __is_true(n >= 0)");
    __ghost(assume, "P := __is_true(cn >= 0)");
    __ghost(group_collapse,
            "n := n, m := cn, items := fun (i: int) (c: int) -> &d[MINDEX2(n, "
            "cn, i, c)] ~> UninitCell");
    for (int ic = 0; ic < n * cn; ic++) {
      __strict();
      __sreads("s ~> Matrix2(n + 2, cn, S)");
      __xwrites(
          "&d[MINDEX2(n, cn, ic / cn, ic % cn)] ~~> reduce_int_sum(ic / cn, ic "
          "/ cn + 3, fun k -> S(k, ic % cn))");
      __ghost(assume, "P := in_range(ic / cn, 0..n)");
      __ghost(assume, "P := in_range(ic % cn, 0..cn)");
      int sum = 0;
      __ghost(rewrite_linear,
              "inside := fun v -> &sum ~~> v, by := reduce_int_sum_empty(ic / "
              "cn, fun k -> S(k, ic % cn))");
      __ghost(assume, "P := in_range(ic / cn, (ic / cn)..(ic / cn + 3))");
      __ghost(assume, "P := in_range(ic / cn + 1, (ic / cn)..(ic / cn + 3))");
      __ghost(assume, "P := in_range(ic / cn + 2, (ic / cn)..(ic / cn + 3))");
      __ghost(assume, "P := in_range(ic / cn, 0..(n + 2))");
      const __ghost_fn focus = __ghost_begin(
          ro_matrix2_focus, "matrix := s, i := ic / cn, j := ic % cn");
      sum += s[MINDEX2(n + 2, cn, ic / cn, ic % cn)];
      __ghost_end(focus);
      __ghost(in_range_bounds, "x := ic / cn, a := ic / cn",
              "k_ge_i <- lower_bound");
      __ghost(plus1, "n := ic / cn, np1 := ic / cn + 1", "kp1 <- eq");
      __ghost(
          rewrite_linear,
          "inside := fun v -> &sum ~~> v, by := reduce_int_sum_add_right(ic / "
          "cn, ic / cn, fun k -> S(k, ic % cn), k_ge_i, ic / cn + 1, kp1)");
      __ghost(
          rewrite_linear,
          "from := ic / cn + 1, to := ic / cn + 1, inside := fun (k: int) -> "
          "&sum ~~> reduce_int_sum(ic / cn, k, fun k0 -> S(k0, ic % cn))");
      __ghost(assume, "P := in_range(ic / cn + 1, 0..(n + 2))");
      const __ghost_fn focus1 = __ghost_begin(
          ro_matrix2_focus, "matrix := s, i := ic / cn + 1, j := ic % cn");
      sum += s[MINDEX2(n + 2, cn, ic / cn + 1, ic % cn)];
      __ghost_end(focus1);
      __ghost(in_range_bounds, "x := ic / cn + 1, a := ic / cn",
              "k_ge_i2 <- lower_bound");
      __ghost(plus1, "n := ic / cn + 1, np1 := ic / cn + 2", "kp13 <- eq");
      __ghost(rewrite_linear,
              "inside := fun v -> &sum ~~> v, by := "
              "reduce_int_sum_add_right(ic / cn, ic / cn + 1, fun k -> S(k, ic "
              "% cn), k_ge_i2, ic / cn + 2, kp13)");
      __ghost(
          rewrite_linear,
          "from := ic / cn + 2, to := ic / cn + 2, inside := fun (k: int) -> "
          "&sum ~~> reduce_int_sum(ic / cn, k, fun k0 -> S(k0, ic % cn))");
      __ghost(assume, "P := in_range(ic / cn + 2, 0..(n + 2))");
      const __ghost_fn focus4 = __ghost_begin(
          ro_matrix2_focus, "matrix := s, i := ic / cn + 2, j := ic % cn");
      sum += s[MINDEX2(n + 2, cn, ic / cn + 2, ic % cn)];
      __ghost_end(focus4);
      __ghost(in_range_bounds, "x := ic / cn + 2, a := ic / cn",
              "k_ge_i5 <- lower_bound");
      __ghost(plus1, "n := ic / cn + 2, np1 := ic / cn + 3", "kp16 <- eq");
      __ghost(rewrite_linear,
              "inside := fun v -> &sum ~~> v, by := "
              "reduce_int_sum_add_right(ic / cn, ic / cn + 2, fun k -> S(k, ic "
              "% cn), k_ge_i5, ic / cn + 3, kp16)");
      __ghost(
          rewrite_linear,
          "from := ic / cn + 3, to := ic / cn + 3, inside := fun (k: int) -> "
          "&sum ~~> reduce_int_sum(ic / cn, k, fun k0 -> S(k0, ic % cn))");
      d[MINDEX2(n, cn, ic / cn, ic % cn)] = sum;
    }
    __ghost(group_uncollapse,
            "n := n, m := cn, items := fun (i: int) (c: int) -> &d[MINDEX2(n, "
            "cn, i, c)] ~~> reduce_int_sum(i, i + 3, fun k -> S(k, c))");
    __ghost([&]() {
      __requires("__is_true(3 >= 0)");
      __requires("__is_true(3 == 3)");
      __consumes("_RO(#_4, s ~> Matrix2(n + 2, cn, S))");
      __consumes(
          "for i in 0..n -> for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> "
          "reduce_int_sum(i, i + 3, fun k -> S(k, c))");
      __ensures("__is_true(w >= 0)");
      __ensures("__is_true(w == 3)");
      __produces("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
      __produces(
          "for i in 0..n -> for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> "
          "reduce_int_sum(i, i + w, fun k -> S(k, c))");
      __admitted();
    });
  }
}
