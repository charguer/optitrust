#include <optitrust_models.h>

__ghost_ret plus1() {
  __requires("n: int");
  __requires("np1: int");
  __requires("__is_true(np1 == n + 1)");
  __ensures("eq: __is_true(np1 == n + 1)");
}

void rowSum(int w, int* s, int* d, int n, int cn) {
  __requires("S: int * int -> int");
  __requires("__is_true(w >= 0)");
  __requires("__is_true(n >= 1)");
  __requires("__is_true(cn >= 0)");
  __writes(
      "d ~> Matrix2(n, cn, fun (i: int) (c: int) -> reduce_int_sum(i, i + w, "
      "fun k -> S(k, c)))");
  __reads("s ~> Matrix2(n + w - 1, cn, S)");
  if (w == 3) /*@w*/ {
    __ghost(assert_alias, "x := w, y := 3");
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
      for (int k = ic / cn; k < ic / cn + 3; k++) {
        __strict();
        __spreserves(
            "&sum ~~> reduce_int_sum(ic / cn, k, fun k0 -> S(k0, ic % cn))");
        __sreads("s ~> Matrix2(n + 2, cn, S)");
        __ghost(assume, "P := in_range(k, 0..(n + 2))");
        const __ghost_fn focus = __ghost_begin(
            ro_matrix2_focus, "matrix := s, i := k, j := ic % cn");
        sum += s[MINDEX2(n + 2, cn, k, ic % cn)];
        __ghost_end(focus);
        __ghost(in_range_bounds, "x := k, a := ic / cn",
                "k_ge_i <- lower_bound");
        __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
        __ghost(
            rewrite_linear,
            "inside := fun v -> &sum ~~> v, by := reduce_int_sum_add_right(ic "
            "/ cn, k, fun k -> S(k, ic % cn), k_ge_i, k + 1, kp1)");
      }
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
  } /*w@*/
  else {
    if (w == 5) /*@w*/ {
      __ghost(assert_alias, "x := w, y := 5");
      __ghost([&]() {
        __requires("__is_true(w >= 0)");
        __requires("__is_false(w == 3)");
        __requires("__is_true(w == 5)");
        __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
        __ensures("__is_true(5 >= 0)");
        __ensures("__is_false(5 == 3)");
        __ensures("__is_true(5 == 5)");
        __produces("_RO(#_4, s ~> Matrix2(n + 4, cn, S))");
        __admitted();
      });
      __ghost(assume, "P := __is_true(n >= 0)");
      __ghost(assume, "P := __is_true(cn >= 0)");
      __ghost(group_collapse,
              "n := n, m := cn, items := fun (i: int) (c: int) -> "
              "&d[MINDEX2(n, cn, i, c)] ~> UninitCell");
      for (int ic = 0; ic < n * cn; ic++) {
        __strict();
        __sreads("s ~> Matrix2(n + 4, cn, S)");
        __xwrites(
            "&d[MINDEX2(n, cn, ic / cn, ic % cn)] ~~> reduce_int_sum(ic / cn, "
            "ic / cn + 5, fun k -> S(k, ic % cn))");
        __ghost(assume, "P := in_range(ic / cn, 0..n)");
        __ghost(assume, "P := in_range(ic % cn, 0..cn)");
        int sum = 0;
        __ghost(rewrite_linear,
                "inside := fun v -> &sum ~~> v, by := reduce_int_sum_empty(ic "
                "/ cn, fun k -> S(k, ic % cn))");
        for (int k = ic / cn; k < ic / cn + 5; k++) {
          __strict();
          __spreserves(
              "&sum ~~> reduce_int_sum(ic / cn, k, fun k0 -> S(k0, ic % cn))");
          __sreads("s ~> Matrix2(n + 4, cn, S)");
          __ghost(assume, "P := in_range(k, 0..(n + 4))");
          const __ghost_fn focus = __ghost_begin(
              ro_matrix2_focus, "matrix := s, i := k, j := ic % cn");
          sum += s[MINDEX2(n + 4, cn, k, ic % cn)];
          __ghost_end(focus);
          __ghost(in_range_bounds, "x := k, a := ic / cn",
                  "k_ge_i <- lower_bound");
          __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
          __ghost(rewrite_linear,
                  "inside := fun v -> &sum ~~> v, by := "
                  "reduce_int_sum_add_right(ic / cn, k, fun k -> S(k, ic % "
                  "cn), k_ge_i, k + 1, kp1)");
        }
        d[MINDEX2(n, cn, ic / cn, ic % cn)] = sum;
      }
      __ghost(
          group_uncollapse,
          "n := n, m := cn, items := fun (i: int) (c: int) -> &d[MINDEX2(n, "
          "cn, i, c)] ~~> reduce_int_sum(i, i + 5, fun k -> S(k, c))");
      __ghost([&]() {
        __requires("__is_true(5 >= 0)");
        __requires("__is_false(5 == 3)");
        __requires("__is_true(5 == 5)");
        __consumes("_RO(#_4, s ~> Matrix2(n + 4, cn, S))");
        __consumes(
            "for i in 0..n -> for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> "
            "reduce_int_sum(i, i + 5, fun k -> S(k, c))");
        __ensures("__is_true(w >= 0)");
        __ensures("__is_false(w == 3)");
        __ensures("__is_true(w == 5)");
        __produces("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
        __produces(
            "for i in 0..n -> for c in 0..cn -> &d[MINDEX2(n, cn, i, c)] ~~> "
            "reduce_int_sum(i, i + w, fun k -> S(k, c))");
        __admitted();
      });
    } /*w@*/
    else /*@anyw*/ {
      __ghost(swap_groups,
              "outer_range := 0..n, inner_range := 0..cn, items := fun (i: "
              "int) (c: int) -> &d[MINDEX2(n, cn, i, c)] ~> UninitCell");
      if (cn == 1) /*@cn*/ {
        __ghost(assert_alias, "x := cn, y := 1");
        __ghost([&]() {
          __requires("__is_true(cn >= 0)");
          __requires("__is_true(cn == 1)");
          __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
          __consumes(
              "for j in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, j)] ~> "
              "UninitCell");
          __ensures("__is_true(1 >= 0)");
          __ensures("__is_true(1 == 1)");
          __produces("_RO(#_4, s ~> Matrix2(n + w - 1, 1, S))");
          __produces(
              "for j in 0..1 -> for i in 0..n -> &d[MINDEX2(n, 1, i, j)] ~> "
              "UninitCell");
          __admitted();
        });
        __ghost(assume, "P := in_range(0, 0..1)");
        __ghost([&]() {
          __consumes(
              "for c in 0..1 -> for i in 0..n -> &d[MINDEX2(n, 1, i, c)] ~> "
              "UninitCell");
          __produces("for i in 0..n -> &d[MINDEX2(n, 1, i, 0)] ~> UninitCell");
          __admitted();
          __with("justif := unroll");
        });
        __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
        __ghost(group_split,
                "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                "fun (i: int) -> &d[MINDEX2(n, 1, i, 0)] ~> UninitCell");
        __ghost(assume, "P := in_range(0, 0..(0 + 1))");
        __ghost([&]() {
          __consumes(
              "for i in 0..(0 + 1) -> &d[MINDEX2(n, 1, i, 0)] ~> UninitCell");
          __produces("&d[MINDEX2(n, 1, 0, 0)] ~> UninitCell");
          __admitted();
          __with("justif := unroll");
        });
        __ghost(assume, "P := in_range(0, 0..n)");
        int sum = 0;
        __ghost(rewrite_linear,
                "inside := fun v -> &sum ~~> v, by := reduce_int_sum_empty(0, "
                "fun k -> S(k, 0))");
        for (int k = 0; k < 0 + w; k++) {
          __strict();
          __spreserves("&sum ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 0))");
          __sreads("s ~> Matrix2(n + w - 1, 1, S)");
          __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
          const __ghost_fn focus =
              __ghost_begin(ro_matrix2_focus, "matrix := s, i := k, j := 0");
          sum += s[MINDEX2(n + w - 1, 1, k, 0)];
          __ghost_end(focus);
          __ghost(in_range_bounds, "x := k, a := 0", "k_ge_i <- lower_bound");
          __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
          __ghost(rewrite_linear,
                  "inside := fun v -> &sum ~~> v, by := "
                  "reduce_int_sum_add_right(0, k, fun k -> S(k, 0), k_ge_i, k "
                  "+ 1, kp1)");
        }
        d[MINDEX2(n, 1, 0, 0)] = sum;
        __ghost([&]() {
          __consumes(
              "&d[MINDEX2(n, 1, 0, 0)] ~~> reduce_int_sum(0, 0 + w, fun k -> "
              "S(k, 0))");
          __produces(
              "for i in 0..(0 + 1) -> &d[MINDEX2(n, 1, i, 0)] ~~> "
              "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
          __admitted();
          __with("justif := roll");
        });
        __ghost(to_prove, "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                "H <- H");
        __ghost(to_prove,
                "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                "H2 <- H");
        __ghost(rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(v, "
                "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 0)), by := H");
        __ghost(rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(0 + 1 "
                "- 1, v, fun k0 -> S(k0, 0)), by := H2");
        for (int i = 0 + 1; i < n; i += 1) {
          __strict();
          __spreserves(
              "&sum ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, 0))");
          __sreads("s ~> Matrix2(n + w - 1, 1, S)");
          __xwrites(
              "&d[MINDEX2(n, 1, i, 0)] ~~> reduce_int_sum(i, i + w, fun k0 -> "
              "S(k0, 0))");
          __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
          __ghost(to_prove, "P := in_range(0, 0..1)");
          const __ghost_fn __ghost_pair_1 = __ghost_begin(
              ro_matrix2_focus, "matrix := s, i := i - 1, j := 0");
          __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
          __ghost(to_prove, "P := in_range(0, 0..1)");
          const __ghost_fn __ghost_pair_2 = __ghost_begin(
              ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 0");
          sum += s[MINDEX2(n + w - 1, 1, i + w - 1, 0)] -
                 s[MINDEX2(n + w - 1, 1, i - 1, 0)];
          __ghost_end(__ghost_pair_2);
          __ghost_end(__ghost_pair_1);
          __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
          __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                  "H3 <- H");
          __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                  "H4 <- H");
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum ~~> v, by := "
                  "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 + w "
                  "- 1, fun k0 -> S(k0, 0), H, H3, H4)");
          d[MINDEX2(n, 1, i, 0)] = sum;
          __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
          __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                  "H6 <- H");
          __ghost(
              rewrite_linear,
              "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 1, i, 0)] ~~> "
              "reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, 0)), by := H5");
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 1, i, 0)] ~~> "
                  "reduce_int_sum(i, v, fun k0 -> S(k0, 0)), by := H6");
        }
        __ghost(group_join,
                "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                "fun (i: int) -> &d[MINDEX2(n, 1, i, 0)] ~~> reduce_int_sum(i, "
                "i + w, fun k -> S(k, 0))");
        __ghost([&]() {
          __consumes(
              "for i in 0..n -> &d[MINDEX2(n, 1, i, 0)] ~~> reduce_int_sum(i, "
              "i + w, fun k -> S(k, 0))");
          __produces(
              "for c in 0..1 -> for i in 0..n -> &d[MINDEX2(n, 1, i, c)] ~~> "
              "reduce_int_sum(i, i + w, fun k -> S(k, c))");
          __admitted();
          __with("justif := roll");
        });
        __ghost([&]() {
          __requires("__is_true(1 >= 0)");
          __requires("__is_true(1 == 1)");
          __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, 1, S))");
          __consumes(
              "for c in 0..1 -> for i in 0..n -> &d[MINDEX2(n, 1, i, c)] ~~> "
              "reduce_int_sum(i, i + w, fun k -> S(k, c))");
          __ensures("__is_true(cn >= 0)");
          __ensures("__is_true(cn == 1)");
          __produces("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
          __produces(
              "for c in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, c)] ~~> "
              "reduce_int_sum(i, i + w, fun k -> S(k, c))");
          __admitted();
        });
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          __ghost(assert_alias, "x := cn, y := 3");
          __ghost([&]() {
            __requires("__is_true(cn >= 0)");
            __requires("__is_false(cn == 1)");
            __requires("__is_true(cn == 3)");
            __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
            __consumes(
                "for j in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, j)] "
                "~> UninitCell");
            __ensures("__is_true(3 >= 0)");
            __ensures("__is_false(3 == 1)");
            __ensures("__is_true(3 == 3)");
            __produces("_RO(#_4, s ~> Matrix2(n + w - 1, 3, S))");
            __produces(
                "for j in 0..3 -> for i in 0..n -> &d[MINDEX2(n, 3, i, j)] ~> "
                "UninitCell");
            __admitted();
          });
          __ghost(assume, "P := in_range(0, 0..3)");
          __ghost(assume, "P := in_range(1, 0..3)");
          __ghost(assume, "P := in_range(2, 0..3)");
          __ghost([&]() {
            __consumes(
                "for c in 0..3 -> for i in 0..n -> &d[MINDEX2(n, 3, i, c)] ~> "
                "UninitCell");
            __produces(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 0)] ~> UninitCell");
            __produces(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 1)] ~> UninitCell");
            __produces(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 2)] ~> UninitCell");
            __admitted();
            __with("justif := unroll");
          });
          __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 0)] ~> UninitCell");
          __ghost(assume, "P := in_range(0, 0..(0 + 1))");
          __ghost([&]() {
            __consumes(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 0)] ~> UninitCell");
            __produces("&d[MINDEX2(n, 3, 0, 0)] ~> UninitCell");
            __admitted();
            __with("justif := unroll");
          });
          __ghost(assume, "P := in_range(0, 0..n)");
          int sum = 0;
          __ghost(rewrite_linear,
                  "inside := fun v -> &sum ~~> v, by := "
                  "reduce_int_sum_empty(0, fun k -> S(k, 0))");
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                  "H <- H");
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                  "H2 <- H");
          __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 1)] ~> UninitCell");
          __ghost(assume, "P := in_range(0, 0..(0 + 1))");
          __ghost([&]() {
            __consumes(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 1)] ~> UninitCell");
            __produces("&d[MINDEX2(n, 3, 0, 1)] ~> UninitCell");
            __admitted();
            __with("justif := unroll");
          });
          __ghost(assume, "P := in_range(0, 0..n)");
          int sum7 = 0;
          __ghost(rewrite_linear,
                  "inside := fun v -> &sum7 ~~> v, by := "
                  "reduce_int_sum_empty(0, fun k -> S(k, 1))");
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                  "H8 <- H");
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                  "H29 <- H");
          __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 2)] ~> UninitCell");
          __ghost(assume, "P := in_range(0, 0..(0 + 1))");
          __ghost([&]() {
            __consumes(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 2)] ~> UninitCell");
            __produces("&d[MINDEX2(n, 3, 0, 2)] ~> UninitCell");
            __admitted();
            __with("justif := unroll");
          });
          __ghost(assume, "P := in_range(0, 0..n)");
          int sum10 = 0;
          __ghost(rewrite_linear,
                  "inside := fun v -> &sum10 ~~> v, by := "
                  "reduce_int_sum_empty(0, fun k -> S(k, 2))");
          for (int k = 0; k < 0 + w; k++) {
            __strict();
            __spreserves("&sum ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 0))");
            __spreserves("&sum7 ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 1))");
            __spreserves("&sum10 ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 2))");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
            const __ghost_fn focus =
                __ghost_begin(ro_matrix2_focus, "matrix := s, i := k, j := 0");
            sum += s[MINDEX2(n + w - 1, 3, k, 0)];
            __ghost_end(focus);
            __ghost(in_range_bounds, "x := k, a := 0", "k_ge_i <- lower_bound");
            __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum ~~> v, by := "
                    "reduce_int_sum_add_right(0, k, fun k -> S(k, 0), k_ge_i, "
                    "k + 1, kp1)");
            __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
            const __ghost_fn focus39 =
                __ghost_begin(ro_matrix2_focus, "matrix := s, i := k, j := 1");
            sum7 += s[MINDEX2(n + w - 1, 3, k, 1)];
            __ghost_end(focus39);
            __ghost(in_range_bounds, "x := k, a := 0",
                    "k_ge_i40 <- lower_bound");
            __ghost(plus1, "n := k, np1 := k + 1", "kp141 <- eq");
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum7 ~~> v, by := "
                    "reduce_int_sum_add_right(0, k, fun k -> S(k, 1), "
                    "k_ge_i40, k + 1, kp141)");
            __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
            const __ghost_fn focus36 =
                __ghost_begin(ro_matrix2_focus, "matrix := s, i := k, j := 2");
            sum10 += s[MINDEX2(n + w - 1, 3, k, 2)];
            __ghost_end(focus36);
            __ghost(in_range_bounds, "x := k, a := 0",
                    "k_ge_i37 <- lower_bound");
            __ghost(plus1, "n := k, np1 := k + 1", "kp138 <- eq");
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum10 ~~> v, by := "
                    "reduce_int_sum_add_right(0, k, fun k -> S(k, 2), "
                    "k_ge_i37, k + 1, kp138)");
          }
          d[MINDEX2(n, 3, 0, 0)] = sum;
          __ghost([&]() {
            __consumes(
                "&d[MINDEX2(n, 3, 0, 0)] ~~> reduce_int_sum(0, 0 + w, fun k -> "
                "S(k, 0))");
            __produces(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 0)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
            __admitted();
            __with("justif := roll");
          });
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(v, "
                  "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 0)), by := H");
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(0 + "
                  "1 - 1, v, fun k0 -> S(k0, 0)), by := H2");
          d[MINDEX2(n, 3, 0, 1)] = sum7;
          __ghost([&]() {
            __consumes(
                "&d[MINDEX2(n, 3, 0, 1)] ~~> reduce_int_sum(0, 0 + w, fun k -> "
                "S(k, 1))");
            __produces(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 1)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
            __admitted();
            __with("justif := roll");
          });
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum7 ~~> reduce_int_sum(v, "
                  "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 1)), by := H8");
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum7 ~~> reduce_int_sum(0 "
                  "+ 1 - 1, v, fun k0 -> S(k0, 1)), by := H29");
          d[MINDEX2(n, 3, 0, 2)] = sum10;
          __ghost([&]() {
            __consumes(
                "&d[MINDEX2(n, 3, 0, 2)] ~~> reduce_int_sum(0, 0 + w, fun k -> "
                "S(k, 2))");
            __produces(
                "for i in 0..(0 + 1) -> &d[MINDEX2(n, 3, i, 2)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
            __admitted();
            __with("justif := roll");
          });
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                  "H11 <- H");
          __ghost(to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                  "H212 <- H");
          __ghost(
              rewrite_linear,
              "inside := [&] (int v) -> HProp  &sum10 ~~> reduce_int_sum(v, "
              "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 2)), by := H11");
          __ghost(rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum10 ~~> reduce_int_sum(0 "
                  "+ 1 - 1, v, fun k0 -> S(k0, 2)), by := H212");
          for (int i = 0 + 1; i < n; i += 1) {
            __strict();
            __spreserves(
                "&sum ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                "0))");
            __spreserves(
                "&sum7 ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                "1))");
            __spreserves(
                "&sum10 ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                "2))");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __sreads("s ~> Matrix2(n + w - 1, 3, S)");
            __xwrites(
                "&d[MINDEX2(n, 3, i, 0)] ~~> reduce_int_sum(i, i + w, fun k0 "
                "-> S(k0, 0))");
            __xwrites(
                "&d[MINDEX2(n, 3, i, 1)] ~~> reduce_int_sum(i, i + w, fun k0 "
                "-> S(k0, 1))");
            __xwrites(
                "&d[MINDEX2(n, 3, i, 2)] ~~> reduce_int_sum(i, i + w, fun k0 "
                "-> S(k0, 2))");
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(0, 0..3)");
            const __ghost_fn __ghost_pair_1 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i - 1, j := 0");
            __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(0, 0..3)");
            const __ghost_fn __ghost_pair_2 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 0");
            sum += s[MINDEX2(n + w - 1, 3, i + w - 1, 0)] -
                   s[MINDEX2(n + w - 1, 3, i - 1, 0)];
            __ghost_end(__ghost_pair_2);
            __ghost_end(__ghost_pair_1);
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H3 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H4 <- H");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &sum ~~> v, by := "
                    "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 + "
                    "w - 1, fun k0 -> S(k0, 0), H, H3, H4)");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H6 <- H");
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(1, 0..3)");
            const __ghost_fn __ghost_pair_129 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i - 1, j := 1");
            __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(1, 0..3)");
            const __ghost_fn __ghost_pair_230 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 1");
            sum7 += s[MINDEX2(n + w - 1, 3, i + w - 1, 1)] -
                    s[MINDEX2(n + w - 1, 3, i - 1, 1)];
            __ghost_end(__ghost_pair_230);
            __ghost_end(__ghost_pair_129);
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H31 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H332 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H433 <- H");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &sum7 ~~> v, by := "
                    "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 + "
                    "w - 1, fun k0 -> S(k0, 1), H31, H332, H433)");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H534 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H635 <- H");
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(2, 0..3)");
            const __ghost_fn __ghost_pair_122 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i - 1, j := 2");
            __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(2, 0..3)");
            const __ghost_fn __ghost_pair_223 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 2");
            sum10 += s[MINDEX2(n + w - 1, 3, i + w - 1, 2)] -
                     s[MINDEX2(n + w - 1, 3, i - 1, 2)];
            __ghost_end(__ghost_pair_223);
            __ghost_end(__ghost_pair_122);
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H24 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H325 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H426 <- H");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &sum10 ~~> v, by := "
                    "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 + "
                    "w - 1, fun k0 -> S(k0, 2), H24, H325, H426)");
            d[MINDEX2(n, 3, i, 0)] = sum;
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 0)] "
                    "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, 0)), "
                    "by := H5");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 0)] "
                    "~~> reduce_int_sum(i, v, fun k0 -> S(k0, 0)), by := H6");
            d[MINDEX2(n, 3, i, 1)] = sum7;
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 1)] "
                    "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, 1)), "
                    "by := H534");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 1)] "
                    "~~> reduce_int_sum(i, v, fun k0 -> S(k0, 1)), by := H635");
            d[MINDEX2(n, 3, i, 2)] = sum10;
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H527 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H628 <- H");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 2)] "
                    "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, 2)), "
                    "by := H527");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 3, i, 2)] "
                    "~~> reduce_int_sum(i, v, fun k0 -> S(k0, 2)), by := H628");
          }
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 0)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 1)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, 3, i, 2)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
          __ghost([&]() {
            __consumes(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 0)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
            __consumes(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 1)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
            __consumes(
                "for i in 0..n -> &d[MINDEX2(n, 3, i, 2)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
            __produces(
                "for c in 0..3 -> for i in 0..n -> &d[MINDEX2(n, 3, i, c)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, c))");
            __admitted();
            __with("justif := roll");
          });
          __ghost([&]() {
            __requires("__is_true(3 >= 0)");
            __requires("__is_false(3 == 1)");
            __requires("__is_true(3 == 3)");
            __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, 3, S))");
            __consumes(
                "for c in 0..3 -> for i in 0..n -> &d[MINDEX2(n, 3, i, c)] ~~> "
                "reduce_int_sum(i, i + w, fun k -> S(k, c))");
            __ensures("__is_true(cn >= 0)");
            __ensures("__is_false(cn == 1)");
            __ensures("__is_true(cn == 3)");
            __produces("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
            __produces(
                "for c in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, c)] "
                "~~> reduce_int_sum(i, i + w, fun k -> S(k, c))");
            __admitted();
          });
        } /*cn@*/
        else {
          if (cn == 4) /*@cn*/ {
            __ghost(assert_alias, "x := cn, y := 4");
            __ghost([&]() {
              __requires("__is_true(cn >= 0)");
              __requires("__is_false(cn == 1)");
              __requires("__is_false(cn == 3)");
              __requires("__is_true(cn == 4)");
              __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
              __consumes(
                  "for j in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, j)] "
                  "~> UninitCell");
              __ensures("__is_true(4 >= 0)");
              __ensures("__is_false(4 == 1)");
              __ensures("__is_false(4 == 3)");
              __ensures("__is_true(4 == 4)");
              __produces("_RO(#_4, s ~> Matrix2(n + w - 1, 4, S))");
              __produces(
                  "for j in 0..4 -> for i in 0..n -> &d[MINDEX2(n, 4, i, j)] "
                  "~> UninitCell");
              __admitted();
            });
            __ghost(assume, "P := in_range(0, 0..4)");
            __ghost(assume, "P := in_range(1, 0..4)");
            __ghost(assume, "P := in_range(2, 0..4)");
            __ghost(assume, "P := in_range(3, 0..4)");
            __ghost([&]() {
              __consumes(
                  "for c in 0..4 -> for i in 0..n -> &d[MINDEX2(n, 4, i, c)] "
                  "~> UninitCell");
              __produces(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 0)] ~> UninitCell");
              __produces(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 1)] ~> UninitCell");
              __produces(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 2)] ~> UninitCell");
              __produces(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 3)] ~> UninitCell");
              __admitted();
              __with("justif := unroll");
            });
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 0)] ~> UninitCell");
            __ghost(assume, "P := in_range(0, 0..(0 + 1))");
            __ghost([&]() {
              __consumes(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 0)] ~> "
                  "UninitCell");
              __produces("&d[MINDEX2(n, 4, 0, 0)] ~> UninitCell");
              __admitted();
              __with("justif := unroll");
            });
            __ghost(assume, "P := in_range(0, 0..n)");
            int sum = 0;
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum ~~> v, by := "
                    "reduce_int_sum_empty(0, fun k -> S(k, 0))");
            __ghost(to_prove,
                    "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                    "H <- H");
            __ghost(
                to_prove,
                "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                "H2 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 1)] ~> UninitCell");
            __ghost(assume, "P := in_range(0, 0..(0 + 1))");
            __ghost([&]() {
              __consumes(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 1)] ~> "
                  "UninitCell");
              __produces("&d[MINDEX2(n, 4, 0, 1)] ~> UninitCell");
              __admitted();
              __with("justif := unroll");
            });
            __ghost(assume, "P := in_range(0, 0..n)");
            int sum13 = 0;
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum13 ~~> v, by := "
                    "reduce_int_sum_empty(0, fun k -> S(k, 1))");
            __ghost(to_prove,
                    "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                    "H14 <- H");
            __ghost(
                to_prove,
                "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                "H215 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 2)] ~> UninitCell");
            __ghost(assume, "P := in_range(0, 0..(0 + 1))");
            __ghost([&]() {
              __consumes(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 2)] ~> "
                  "UninitCell");
              __produces("&d[MINDEX2(n, 4, 0, 2)] ~> UninitCell");
              __admitted();
              __with("justif := unroll");
            });
            __ghost(assume, "P := in_range(0, 0..n)");
            int sum16 = 0;
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum16 ~~> v, by := "
                    "reduce_int_sum_empty(0, fun k -> S(k, 2))");
            __ghost(to_prove,
                    "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                    "H17 <- H");
            __ghost(
                to_prove,
                "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                "H218 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 3)] ~> UninitCell");
            __ghost(assume, "P := in_range(0, 0..(0 + 1))");
            __ghost([&]() {
              __consumes(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 3)] ~> "
                  "UninitCell");
              __produces("&d[MINDEX2(n, 4, 0, 3)] ~> UninitCell");
              __admitted();
              __with("justif := unroll");
            });
            __ghost(assume, "P := in_range(0, 0..n)");
            int sum19 = 0;
            __ghost(rewrite_linear,
                    "inside := fun v -> &sum19 ~~> v, by := "
                    "reduce_int_sum_empty(0, fun k -> S(k, 3))");
            for (int k = 0; k < 0 + w; k++) {
              __strict();
              __spreserves("&sum ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 0))");
              __spreserves(
                  "&sum13 ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 1))");
              __spreserves(
                  "&sum16 ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 2))");
              __spreserves(
                  "&sum19 ~~> reduce_int_sum(0, k, fun k0 -> S(k0, 3))");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
              const __ghost_fn focus = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := k, j := 0");
              sum += s[MINDEX2(n + w - 1, 4, k, 0)];
              __ghost_end(focus);
              __ghost(in_range_bounds, "x := k, a := 0",
                      "k_ge_i <- lower_bound");
              __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
              __ghost(rewrite_linear,
                      "inside := fun v -> &sum ~~> v, by := "
                      "reduce_int_sum_add_right(0, k, fun k -> S(k, 0), "
                      "k_ge_i, k + 1, kp1)");
              __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
              const __ghost_fn focus69 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := k, j := 1");
              sum13 += s[MINDEX2(n + w - 1, 4, k, 1)];
              __ghost_end(focus69);
              __ghost(in_range_bounds, "x := k, a := 0",
                      "k_ge_i70 <- lower_bound");
              __ghost(plus1, "n := k, np1 := k + 1", "kp171 <- eq");
              __ghost(rewrite_linear,
                      "inside := fun v -> &sum13 ~~> v, by := "
                      "reduce_int_sum_add_right(0, k, fun k -> S(k, 1), "
                      "k_ge_i70, k + 1, kp171)");
              __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
              const __ghost_fn focus66 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := k, j := 2");
              sum16 += s[MINDEX2(n + w - 1, 4, k, 2)];
              __ghost_end(focus66);
              __ghost(in_range_bounds, "x := k, a := 0",
                      "k_ge_i67 <- lower_bound");
              __ghost(plus1, "n := k, np1 := k + 1", "kp168 <- eq");
              __ghost(rewrite_linear,
                      "inside := fun v -> &sum16 ~~> v, by := "
                      "reduce_int_sum_add_right(0, k, fun k -> S(k, 2), "
                      "k_ge_i67, k + 1, kp168)");
              __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
              const __ghost_fn focus63 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := k, j := 3");
              sum19 += s[MINDEX2(n + w - 1, 4, k, 3)];
              __ghost_end(focus63);
              __ghost(in_range_bounds, "x := k, a := 0",
                      "k_ge_i64 <- lower_bound");
              __ghost(plus1, "n := k, np1 := k + 1", "kp165 <- eq");
              __ghost(rewrite_linear,
                      "inside := fun v -> &sum19 ~~> v, by := "
                      "reduce_int_sum_add_right(0, k, fun k -> S(k, 3), "
                      "k_ge_i64, k + 1, kp165)");
            }
            d[MINDEX2(n, 4, 0, 0)] = sum;
            __ghost([&]() {
              __consumes(
                  "&d[MINDEX2(n, 4, 0, 0)] ~~> reduce_int_sum(0, 0 + w, fun k "
                  "-> S(k, 0))");
              __produces(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 0)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
              __admitted();
              __with("justif := roll");
            });
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(v, "
                "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 0)), by := H");
            __ghost(rewrite_linear,
                    "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(0 "
                    "+ 1 - 1, v, fun k0 -> S(k0, 0)), by := H2");
            d[MINDEX2(n, 4, 0, 1)] = sum13;
            __ghost([&]() {
              __consumes(
                  "&d[MINDEX2(n, 4, 0, 1)] ~~> reduce_int_sum(0, 0 + w, fun k "
                  "-> S(k, 1))");
              __produces(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 1)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
              __admitted();
              __with("justif := roll");
            });
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum13 ~~> reduce_int_sum(v, "
                "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 1)), by := H14");
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum13 ~~> reduce_int_sum(0 + "
                "1 - 1, v, fun k0 -> S(k0, 1)), by := H215");
            d[MINDEX2(n, 4, 0, 2)] = sum16;
            __ghost([&]() {
              __consumes(
                  "&d[MINDEX2(n, 4, 0, 2)] ~~> reduce_int_sum(0, 0 + w, fun k "
                  "-> S(k, 2))");
              __produces(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 2)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
              __admitted();
              __with("justif := roll");
            });
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum16 ~~> reduce_int_sum(v, "
                "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 2)), by := H17");
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum16 ~~> reduce_int_sum(0 + "
                "1 - 1, v, fun k0 -> S(k0, 2)), by := H218");
            d[MINDEX2(n, 4, 0, 3)] = sum19;
            __ghost([&]() {
              __consumes(
                  "&d[MINDEX2(n, 4, 0, 3)] ~~> reduce_int_sum(0, 0 + w, fun k "
                  "-> S(k, 3))");
              __produces(
                  "for i in 0..(0 + 1) -> &d[MINDEX2(n, 4, i, 3)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 3))");
              __admitted();
              __with("justif := roll");
            });
            __ghost(to_prove,
                    "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                    "H20 <- H");
            __ghost(
                to_prove,
                "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                "H221 <- H");
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum19 ~~> reduce_int_sum(v, "
                "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, 3)), by := H20");
            __ghost(
                rewrite_linear,
                "inside := [&] (int v) -> HProp  &sum19 ~~> reduce_int_sum(0 + "
                "1 - 1, v, fun k0 -> S(k0, 3)), by := H221");
            for (int i = 0 + 1; i < n; i += 1) {
              __strict();
              __spreserves(
                  "&sum ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                  "0))");
              __spreserves(
                  "&sum13 ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                  "1))");
              __spreserves(
                  "&sum16 ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                  "2))");
              __spreserves(
                  "&sum19 ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                  "3))");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __sreads("s ~> Matrix2(n + w - 1, 4, S)");
              __xwrites(
                  "&d[MINDEX2(n, 4, i, 0)] ~~> reduce_int_sum(i, i + w, fun k0 "
                  "-> S(k0, 0))");
              __xwrites(
                  "&d[MINDEX2(n, 4, i, 1)] ~~> reduce_int_sum(i, i + w, fun k0 "
                  "-> S(k0, 1))");
              __xwrites(
                  "&d[MINDEX2(n, 4, i, 2)] ~~> reduce_int_sum(i, i + w, fun k0 "
                  "-> S(k0, 2))");
              __xwrites(
                  "&d[MINDEX2(n, 4, i, 3)] ~~> reduce_int_sum(i, i + w, fun k0 "
                  "-> S(k0, 3))");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(0, 0..4)");
              const __ghost_fn __ghost_pair_1 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 0");
              __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(0, 0..4)");
              const __ghost_fn __ghost_pair_2 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 0");
              sum += s[MINDEX2(n + w - 1, 4, i + w - 1, 0)] -
                     s[MINDEX2(n + w - 1, 4, i - 1, 0)];
              __ghost_end(__ghost_pair_2);
              __ghost_end(__ghost_pair_1);
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H3 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H4 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &sum ~~> v, by := "
                      "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 "
                      "+ w - 1, fun k0 -> S(k0, 0), H, H3, H4)");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H6 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(1, 0..4)");
              const __ghost_fn __ghost_pair_156 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 1");
              __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(1, 0..4)");
              const __ghost_fn __ghost_pair_257 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 1");
              sum13 += s[MINDEX2(n + w - 1, 4, i + w - 1, 1)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 1)];
              __ghost_end(__ghost_pair_257);
              __ghost_end(__ghost_pair_156);
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H58 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H359 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H460 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &sum13 ~~> v, by := "
                      "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 "
                      "+ w - 1, fun k0 -> S(k0, 1), H58, H359, H460)");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H561 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H662 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(2, 0..4)");
              const __ghost_fn __ghost_pair_149 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 2");
              __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(2, 0..4)");
              const __ghost_fn __ghost_pair_250 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 2");
              sum16 += s[MINDEX2(n + w - 1, 4, i + w - 1, 2)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 2)];
              __ghost_end(__ghost_pair_250);
              __ghost_end(__ghost_pair_149);
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H51 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H352 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H453 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &sum16 ~~> v, by := "
                      "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 "
                      "+ w - 1, fun k0 -> S(k0, 2), H51, H352, H453)");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H554 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H655 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(3, 0..4)");
              const __ghost_fn __ghost_pair_142 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 3");
              __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(3, 0..4)");
              const __ghost_fn __ghost_pair_243 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i + w - 1, j := 3");
              sum19 += s[MINDEX2(n + w - 1, 4, i + w - 1, 3)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 3)];
              __ghost_end(__ghost_pair_243);
              __ghost_end(__ghost_pair_142);
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H44 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H345 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H446 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &sum19 ~~> v, by := "
                      "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 "
                      "+ w - 1, fun k0 -> S(k0, 3), H44, H345, H446)");
              d[MINDEX2(n, 4, i, 0)] = sum;
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 0)] "
                      "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, "
                      "0)), by := H5");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 0)] "
                      "~~> reduce_int_sum(i, v, fun k0 -> S(k0, 0)), by := H6");
              d[MINDEX2(n, 4, i, 1)] = sum13;
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 1)] "
                      "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, "
                      "1)), by := H561");
              __ghost(
                  rewrite_linear,
                  "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 1)] ~~> "
                  "reduce_int_sum(i, v, fun k0 -> S(k0, 1)), by := H662");
              d[MINDEX2(n, 4, i, 2)] = sum16;
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 2)] "
                      "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, "
                      "2)), by := H554");
              __ghost(
                  rewrite_linear,
                  "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 2)] ~~> "
                  "reduce_int_sum(i, v, fun k0 -> S(k0, 2)), by := H655");
              d[MINDEX2(n, 4, i, 3)] = sum19;
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H547 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H648 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 3)] "
                      "~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> S(k0, "
                      "3)), by := H547");
              __ghost(
                  rewrite_linear,
                  "inside := [&] (int v) -> HProp  &d[MINDEX2(n, 4, i, 3)] ~~> "
                  "reduce_int_sum(i, v, fun k0 -> S(k0, 3)), by := H648");
            }
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 0)] ~~> "
                    "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 1)] ~~> "
                    "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 2)] ~~> "
                    "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 0 + 1, items "
                    ":= fun (i: int) -> &d[MINDEX2(n, 4, i, 3)] ~~> "
                    "reduce_int_sum(i, i + w, fun k -> S(k, 3))");
            __ghost([&]() {
              __consumes(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 0)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 0))");
              __consumes(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 1)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 1))");
              __consumes(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 2)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 2))");
              __consumes(
                  "for i in 0..n -> &d[MINDEX2(n, 4, i, 3)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, 3))");
              __produces(
                  "for c in 0..4 -> for i in 0..n -> &d[MINDEX2(n, 4, i, c)] "
                  "~~> reduce_int_sum(i, i + w, fun k -> S(k, c))");
              __admitted();
              __with("justif := roll");
            });
            __ghost([&]() {
              __requires("__is_true(4 >= 0)");
              __requires("__is_false(4 == 1)");
              __requires("__is_false(4 == 3)");
              __requires("__is_true(4 == 4)");
              __consumes("_RO(#_4, s ~> Matrix2(n + w - 1, 4, S))");
              __consumes(
                  "for c in 0..4 -> for i in 0..n -> &d[MINDEX2(n, 4, i, c)] "
                  "~~> reduce_int_sum(i, i + w, fun k -> S(k, c))");
              __ensures("__is_true(cn >= 0)");
              __ensures("__is_false(cn == 1)");
              __ensures("__is_false(cn == 3)");
              __ensures("__is_true(cn == 4)");
              __produces("_RO(#_4, s ~> Matrix2(n + w - 1, cn, S))");
              __produces(
                  "for c in 0..cn -> for i in 0..n -> &d[MINDEX2(n, cn, i, c)] "
                  "~~> reduce_int_sum(i, i + w, fun k -> S(k, c))");
              __admitted();
            });
          } /*cn@*/
          else /*@anycn*/ {
            for (int c = 0; c < cn; c++) {
              __strict();
              __sreads("s ~> Matrix2(n + w - 1, cn, S)");
              __xwrites(
                  "for i in 0..n -> &d[MINDEX2(n, cn, i, c)] ~~> "
                  "reduce_int_sum(i, i + w, fun k -> S(k, c))");
              __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
              __ghost(
                  group_split,
                  "start := 0, stop := n, step := 1, split := 0 + 1, items := "
                  "fun (i: int) -> &d[MINDEX2(n, cn, i, c)] ~> UninitCell");
              __ghost(assume, "P := in_range(0, 0..(0 + 1))");
              __ghost([&]() {
                __consumes(
                    "for i in 0..(0 + 1) -> &d[MINDEX2(n, cn, i, c)] ~> "
                    "UninitCell");
                __produces("&d[MINDEX2(n, cn, 0, c)] ~> UninitCell");
                __admitted();
                __with("justif := unroll");
              });
              __ghost(assume, "P := in_range(0, 0..n)");
              int sum = 0;
              __ghost(rewrite_linear,
                      "inside := fun v -> &sum ~~> v, by := "
                      "reduce_int_sum_empty(0, fun k -> S(k, c))");
              for (int k = 0; k < 0 + w; k++) {
                __strict();
                __spreserves(
                    "&sum ~~> reduce_int_sum(0, k, fun k0 -> S(k0, c))");
                __sreads("s ~> Matrix2(n + w - 1, cn, S)");
                __ghost(assume, "P := in_range(k, 0..(n + w - 1))");
                const __ghost_fn focus = __ghost_begin(
                    ro_matrix2_focus, "matrix := s, i := k, j := c");
                sum += s[MINDEX2(n + w - 1, cn, k, c)];
                __ghost_end(focus);
                __ghost(in_range_bounds, "x := k, a := 0",
                        "k_ge_i <- lower_bound");
                __ghost(plus1, "n := k, np1 := k + 1", "kp1 <- eq");
                __ghost(rewrite_linear,
                        "inside := fun v -> &sum ~~> v, by := "
                        "reduce_int_sum_add_right(0, k, fun k -> S(k, c), "
                        "k_ge_i, k + 1, kp1)");
              }
              d[MINDEX2(n, cn, 0, c)] = sum;
              __ghost([&]() {
                __consumes(
                    "&d[MINDEX2(n, cn, 0, c)] ~~> reduce_int_sum(0, 0 + w, fun "
                    "k -> S(k, c))");
                __produces(
                    "for i in 0..(0 + 1) -> &d[MINDEX2(n, cn, i, c)] ~~> "
                    "reduce_int_sum(i, i + w, fun k -> S(k, c))");
                __admitted();
                __with("justif := roll");
              });
              __ghost(to_prove,
                      "P := __is_true(/*@__105*/0/*__105@*/ == 0 + 1 - 1)",
                      "H <- H");
              __ghost(
                  to_prove,
                  "P := __is_true(/*@__105*/0/*__105@*/ + w == 0 + 1 + w - 1)",
                  "H2 <- H");
              __ghost(
                  rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(v, "
                  "/*@__105*/0/*__105@*/ + w, fun k0 -> S(k0, c)), by := H");
              __ghost(
                  rewrite_linear,
                  "inside := [&] (int v) -> HProp  &sum ~~> reduce_int_sum(0 + "
                  "1 - 1, v, fun k0 -> S(k0, c)), by := H2");
              for (int i = 0 + 1; i < n; i += 1) {
                __strict();
                __spreserves(
                    "&sum ~~> reduce_int_sum(i - 1, i + w - 1, fun k0 -> S(k0, "
                    "c))");
                __sreads("s ~> Matrix2(n + w - 1, cn, S)");
                __xwrites(
                    "&d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, i + w, fun "
                    "k0 -> S(k0, c))");
                __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
                __ghost(to_prove, "P := in_range(c, 0..cn)");
                const __ghost_fn __ghost_pair_1 = __ghost_begin(
                    ro_matrix2_focus, "matrix := s, i := i - 1, j := c");
                __ghost(to_prove, "P := in_range(i + w - 1, 0..(n + w - 1))");
                __ghost(to_prove, "P := in_range(c, 0..cn)");
                const __ghost_fn __ghost_pair_2 = __ghost_begin(
                    ro_matrix2_focus, "matrix := s, i := i + w - 1, j := c");
                sum += s[MINDEX2(n + w - 1, cn, i + w - 1, c)] -
                       s[MINDEX2(n + w - 1, cn, i - 1, c)];
                __ghost_end(__ghost_pair_2);
                __ghost_end(__ghost_pair_1);
                __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                        "H <- H");
                __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                        "H3 <- H");
                __ghost(to_prove,
                        "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                        "H4 <- H");
                __ghost(rewrite_linear,
                        "inside := [&] (int v) -> HProp  &sum ~~> v, by := "
                        "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + "
                        "1 + w - 1, fun k0 -> S(k0, c), H, H3, H4)");
                d[MINDEX2(n, cn, i, c)] = sum;
                __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
                __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                        "H6 <- H");
                __ghost(rewrite_linear,
                        "inside := [&] (int v) -> HProp  &d[MINDEX2(n, cn, i, "
                        "c)] ~~> reduce_int_sum(v, i + 1 + w - 1, fun k0 -> "
                        "S(k0, c)), by := H5");
                __ghost(
                    rewrite_linear,
                    "inside := [&] (int v) -> HProp  &d[MINDEX2(n, cn, i, c)] "
                    "~~> reduce_int_sum(i, v, fun k0 -> S(k0, c)), by := H6");
              }
              __ghost(group_join,
                      "start := 0, stop := n, step := 1, split := 0 + 1, items "
                      ":= fun (i: int) -> &d[MINDEX2(n, cn, i, c)] ~~> "
                      "reduce_int_sum(i, i + w, fun k -> S(k, c))");
            }
          } /*anycn@*/
        }
      }
      __ghost(swap_groups,
              "outer_range := 0..cn, inner_range := 0..n, items := fun (c: "
              "int) (i: int) -> &d[MINDEX2(n, cn, i, c)] ~~> reduce_int_sum(i, "
              "i + w, fun k -> S(k, c))");
    } /*anyw@*/
  }
}
