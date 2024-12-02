#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int kn, const uint8_t* S, uint16_t* D, const int n,
            const int cn) {
  __requires("#4: _Fraction");
  __requires("__is_geq(kn, 0)");
  __requires("__is_geq(n, 1)");
  __requires("__is_geq(cn, 0)");
  __modifies("D ~> Matrix2(n, cn)");
  __consumes("RO(#4, S ~> Matrix2(n + kn - 1, cn))");
  __produces("RO(#4, S ~> Matrix2(n + kn - 1, cn))");
  if (kn == 3) /*@kn*/ {
    __ghost(assert_alias, "x := kn, y := 3");
    __ghost(
        [&]() {
          __requires("__is_geq(kn, 0)");
          __requires("__is_eq(kn, 3)");
          __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
          __ensures("__is_geq(3, 0)");
          __ensures("__is_eq(3, 3)");
          __produces("_RO(#_4, S ~> Matrix2(n + 2, cn))");
          __admitted();
        },
        "");
    __ghost(assume, "F := __is_geq(n, 0)");
    __ghost(assume, "F := __is_geq(cn, 0)");
    __ghost(group_collapse,
            "n := n, m := cn, items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> "
            "Cell");
    for (int ic = 0; ic < n * cn; ic++) {
      __strict();
      __sreads("S ~> Matrix2(n + 2, cn)");
      __xmodifies("&D[MINDEX2(n, cn, ic / cn, ic % cn)] ~> Cell");
      __ghost(assume, "F := in_range(ic / cn, 0..n)");
      __ghost(assume, "F := in_range(ic % cn, 0..cn)");
      __ghost(assume, "F := is_subrange(ic / cn..ic / cn + 3, 0..n + 2)");
      __ghost(assume, "F := in_range(ic / cn, ic / cn..ic / cn + 3)");
      __ghost(in_range_extend,
              "x := ic / cn, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_3 = __ghost_begin(
          matrix2_ro_focus,
          "M := S, i := ic / cn, j := ic % cn, m := n + 2, n := cn");
      __ghost(assume, "F := in_range(ic / cn + 1, ic / cn..ic / cn + 3)");
      __ghost(in_range_extend,
              "x := ic / cn + 1, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_2 = __ghost_begin(
          matrix2_ro_focus,
          "M := S, i := ic / cn + 1, j := ic % cn, m := n + 2, n := cn");
      __ghost(assume, "F := in_range(ic / cn + 2, ic / cn..ic / cn + 3)");
      __ghost(in_range_extend,
              "x := ic / cn + 2, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_1 = __ghost_begin(
          matrix2_ro_focus,
          "M := S, i := ic / cn + 2, j := ic % cn, m := n + 2, n := cn");
      D[MINDEX2(n, cn, ic / cn, ic % cn)] =
          (uint16_t)S[MINDEX2(n + 2, cn, ic / cn, ic % cn)] +
          (uint16_t)S[MINDEX2(n + 2, cn, ic / cn + 1, ic % cn)] +
          (uint16_t)S[MINDEX2(n + 2, cn, ic / cn + 2, ic % cn)];
      __ghost_end(__ghost_pair_1);
      __ghost_end(__ghost_pair_2);
      __ghost_end(__ghost_pair_3);
    }
    __ghost(group_uncollapse,
            "n := n, m := cn, items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> "
            "Cell");
    __ghost(
        [&]() {
          __requires("__is_geq(3, 0)");
          __requires("__is_eq(3, 3)");
          __consumes("_RO(#_4, S ~> Matrix2(n + 2, cn))");
          __ensures("__is_geq(kn, 0)");
          __ensures("__is_eq(kn, 3)");
          __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
          __admitted();
        },
        "");
  } /*kn@*/
  else {
    if (kn == 5) /*@kn*/ {
      __ghost(assert_alias, "x := kn, y := 5");
      __ghost(
          [&]() {
            __requires("__is_geq(kn, 0)");
            __requires("__not(__is_eq(kn, 3))");
            __requires("__is_eq(kn, 5)");
            __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
            __ensures("__is_geq(5, 0)");
            __ensures("__not(__is_eq(5, 3))");
            __ensures("__is_eq(5, 5)");
            __produces("_RO(#_4, S ~> Matrix2(n + 4, cn))");
            __admitted();
          },
          "");
      __ghost(assume, "F := __is_geq(n, 0)");
      __ghost(assume, "F := __is_geq(cn, 0)");
      __ghost(group_collapse,
              "n := n, m := cn, items := fun i, c -> &D[MINDEX2(n, cn, i, c)] "
              "~> Cell");
      for (int ic = 0; ic < n * cn; ic++) {
        __strict();
        __sreads("S ~> Matrix2(n + 4, cn)");
        __xmodifies("&D[MINDEX2(n, cn, ic / cn, ic % cn)] ~> Cell");
        __ghost(assume, "F := in_range(ic / cn, 0..n)");
        __ghost(assume, "F := in_range(ic % cn, 0..cn)");
        __ghost(assume, "F := is_subrange(ic / cn..ic / cn + 5, 0..n + 4)");
        __ghost(assume, "F := in_range(ic / cn, ic / cn..ic / cn + 5)");
        __ghost(in_range_extend,
                "x := ic / cn, r1 := ic / cn..ic / cn + 5, r2 := 0..n + 4");
        const __ghost_fn __ghost_pair_8 = __ghost_begin(
            matrix2_ro_focus,
            "M := S, i := ic / cn, j := ic % cn, m := n + 4, n := cn");
        __ghost(assume, "F := in_range(ic / cn + 1, ic / cn..ic / cn + 5)");
        __ghost(in_range_extend,
                "x := ic / cn + 1, r1 := ic / cn..ic / cn + 5, r2 := 0..n + 4");
        const __ghost_fn __ghost_pair_7 = __ghost_begin(
            matrix2_ro_focus,
            "M := S, i := ic / cn + 1, j := ic % cn, m := n + 4, n := cn");
        __ghost(assume, "F := in_range(ic / cn + 2, ic / cn..ic / cn + 5)");
        __ghost(in_range_extend,
                "x := ic / cn + 2, r1 := ic / cn..ic / cn + 5, r2 := 0..n + 4");
        const __ghost_fn __ghost_pair_6 = __ghost_begin(
            matrix2_ro_focus,
            "M := S, i := ic / cn + 2, j := ic % cn, m := n + 4, n := cn");
        __ghost(assume, "F := in_range(ic / cn + 3, ic / cn..ic / cn + 5)");
        __ghost(in_range_extend,
                "x := ic / cn + 3, r1 := ic / cn..ic / cn + 5, r2 := 0..n + 4");
        const __ghost_fn __ghost_pair_5 = __ghost_begin(
            matrix2_ro_focus,
            "M := S, i := ic / cn + 3, j := ic % cn, m := n + 4, n := cn");
        __ghost(assume, "F := in_range(ic / cn + 4, ic / cn..ic / cn + 5)");
        __ghost(in_range_extend,
                "x := ic / cn + 4, r1 := ic / cn..ic / cn + 5, r2 := 0..n + 4");
        const __ghost_fn __ghost_pair_4 = __ghost_begin(
            matrix2_ro_focus,
            "M := S, i := ic / cn + 4, j := ic % cn, m := n + 4, n := cn");
        D[MINDEX2(n, cn, ic / cn, ic % cn)] =
            (uint16_t)S[MINDEX2(n + 4, cn, ic / cn, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 4, cn, ic / cn + 1, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 4, cn, ic / cn + 2, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 4, cn, ic / cn + 3, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 4, cn, ic / cn + 4, ic % cn)];
        __ghost_end(__ghost_pair_4);
        __ghost_end(__ghost_pair_5);
        __ghost_end(__ghost_pair_6);
        __ghost_end(__ghost_pair_7);
        __ghost_end(__ghost_pair_8);
      }
      __ghost(group_uncollapse,
              "n := n, m := cn, items := fun i, c -> &D[MINDEX2(n, cn, i, c)] "
              "~> Cell");
      __ghost(
          [&]() {
            __requires("__is_geq(5, 0)");
            __requires("__not(__is_eq(5, 3))");
            __requires("__is_eq(5, 5)");
            __consumes("_RO(#_4, S ~> Matrix2(n + 4, cn))");
            __ensures("__is_geq(kn, 0)");
            __ensures("__not(__is_eq(kn, 3))");
            __ensures("__is_eq(kn, 5)");
            __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
            __admitted();
          },
          "");
    } /*kn@*/
    else /*@nokn*/ {
      __ghost(swap_groups,
              "outer_range := 0..n, inner_range := 0..cn, items := fun i, c -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
      if (cn == 1) /*@cn*/ {
        __ghost(assert_alias, "x := cn, y := 1");
        __ghost(
            [&]() {
              __requires("__is_geq(cn, 0)");
              __requires("__is_eq(cn, 1)");
              __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
              __consumes(
                  "for j in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, j)] "
                  "~> Cell");
              __ensures("__is_geq(1, 0)");
              __ensures("__is_eq(1, 1)");
              __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, 1))");
              __produces(
                  "for j in 0..1 -> for i in 0..n -> &D[MINDEX2(n, 1, i, j)] "
                  "~> Cell");
              __admitted();
            },
            "");
        __ghost(
            [&]() {
              __consumes(
                  "for c in 0..1 -> for i in 0..n -> &D[MINDEX2(n, 1, i, c)] "
                  "~> Cell");
              __produces("for i in 0..n -> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
              __admitted();
              __with("justif := unroll");
            },
            "");
        __ghost(assume, "F := in_range(1, 0..n)");
        __ghost(group_split,
                "start := 0, stop := n, step := 1, split := 1, items := fun i "
                "-> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
        __ghost(
            [&]() {
              __consumes("for i in 0..1 -> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
              __produces("&D[MINDEX2(n, 1, 0, 0)] ~> Cell");
              __admitted();
              __with("justif := unroll");
            },
            "");
        __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
        uint16_t s = (uint16_t)0;
        __ghost(to_prove, "F := __is_neq(1, 0)");
        for (int i = 0; i < kn; i += 1) {
          __strict();
          __smodifies("&s ~> Cell");
          __sreads("S ~> Matrix2(n + kn - 1, 1)");
          __ghost(assume, "F := in_range(i, 0..kn)");
          __ghost(assume, "F := in_range(i, 0..kn)");
          __ghost(in_range_extend, "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_9 =
              __ghost_begin(matrix2_ro_focus,
                            "M := S, i := i, j := 0, m := n + kn - 1, n := 1");
          s = s + (uint16_t)S[MINDEX2(n + kn - 1, 1, i, 0)];
          __ghost_end(__ghost_pair_9);
        }
        D[MINDEX2(n, 1, 0, 0)] = s;
        __ghost(
            [&]() {
              __consumes("&D[MINDEX2(n, 1, 0, 0)] ~> Cell");
              __produces("for i in 0..1 -> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
              __admitted();
              __with("justif := roll");
            },
            "");
        __ghost(group_shift,
                "start := 1, stop := n, step := 1, items := fun i -> "
                "&D[MINDEX2(n, 1, i, 0)] ~> Cell, shift := - 1, new_start := "
                "0, new_stop := n - 1");
        __ghost(
            [&]() {
              __consumes(
                  "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_1 - (-1), "
                  "0)] ~> Cell");
              __produces(
                  "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_1 + 1, "
                  "0)] ~> Cell");
              __admitted();
              __with("justif := arith_simpl");
            },
            "");
        __ghost(to_prove, "F := __is_neq(1, 0)");
        __ghost(
            [&]() {
              __modifies(
                  "for TMP_2 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_2 + 1, "
                  "0)] ~> Cell");
              __admitted();
              __with("justif := arith_simpl");
            },
            "");
        for (int i = 0; i < n - 1; i += 1) {
          __strict();
          __smodifies("&s ~> Cell");
          __sreads("S ~> Matrix2(n + kn - 1, 1)");
          __xmodifies("&D[MINDEX2(n, 1, i + 1, 0)] ~> Cell");
          __ghost(assume, "F := in_range(i, 0..(n - 1))");
          __ghost(assume, "F := in_range(i + 1, 1..n)");
          __ghost(assume,
                  "F := is_subrange(i + kn..i + 1 + kn, 0..n + kn - 1)");
          __ghost(assume, "F := is_subrange(i..i + 1, 0..n + kn - 1)");
          __ghost(assume, "F := is_subrange(i + 1..i + 1 + kn, 0..n + kn - 1)");
          __ghost(assume, "F := in_range(i + kn, i + kn..i + 1 + kn)");
          __ghost(in_range_extend,
                  "x := i + kn, r1 := i + kn..i + 1 + kn, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_11 = __ghost_begin(
              matrix2_ro_focus,
              "M := S, i := i + kn, j := 0, m := n + kn - 1, n := 1");
          __ghost(assume, "F := in_range(i, i..i + 1)");
          __ghost(in_range_extend,
                  "x := i, r1 := i..i + 1, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_12 =
              __ghost_begin(matrix2_ro_focus,
                            "M := S, i := i, j := 0, m := n + kn - 1, n := 1");
          s = s + (uint16_t)S[MINDEX2(n + kn - 1, 1, i + kn, 0)] -
              (uint16_t)S[MINDEX2(n + kn - 1, 1, i, 0)];
          __ghost_end(__ghost_pair_12);
          __ghost_end(__ghost_pair_11);
          D[MINDEX2(n, 1, i + 1, 0)] = s;
        }
        __ghost(
            [&]() {
              __modifies(
                  "for TMP_2 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_2 + 1, "
                  "0)] ~> Cell");
              __admitted();
              __with("justif := arith_simpl");
            },
            "");
        __ghost(
            [&]() {
              __consumes(
                  "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_1 + 1, "
                  "0)] ~> Cell");
              __produces(
                  "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 1, TMP_1 - (-1), "
                  "0)] ~> Cell");
              __admitted();
              __with("justif := arith_simpl");
            },
            "");
        __ghost(group_unshift,
                "start := 1, stop := n, step := 1, items := fun i -> "
                "&D[MINDEX2(n, 1, i, 0)] ~> Cell, shift := - 1, new_start := "
                "0, new_stop := n - 1");
        __ghost(group_join,
                "start := 0, stop := n, step := 1, split := 1, items := fun i "
                "-> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
        __ghost(
            [&]() {
              __consumes("for i in 0..n -> &D[MINDEX2(n, 1, i, 0)] ~> Cell");
              __produces(
                  "for c in 0..1 -> for i in 0..n -> &D[MINDEX2(n, 1, i, c)] "
                  "~> Cell");
              __admitted();
              __with("justif := roll");
            },
            "");
        __ghost(
            [&]() {
              __requires("__is_geq(1, 0)");
              __requires("__is_eq(1, 1)");
              __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, 1))");
              __consumes(
                  "for c in 0..1 -> for i in 0..n -> &D[MINDEX2(n, 1, i, c)] "
                  "~> Cell");
              __ensures("__is_geq(cn, 0)");
              __ensures("__is_eq(cn, 1)");
              __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
              __produces(
                  "for c in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, c)] "
                  "~> Cell");
              __admitted();
            },
            "");
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          __ghost(assert_alias, "x := cn, y := 3");
          __ghost(
              [&]() {
                __requires("__is_geq(cn, 0)");
                __requires("__not(__is_eq(cn, 1))");
                __requires("__is_eq(cn, 3)");
                __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
                __consumes(
                    "for j in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, "
                    "j)] ~> Cell");
                __ensures("__is_geq(3, 0)");
                __ensures("__not(__is_eq(3, 1))");
                __ensures("__is_eq(3, 3)");
                __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, 3))");
                __produces(
                    "for j in 0..3 -> for i in 0..n -> &D[MINDEX2(n, 3, i, j)] "
                    "~> Cell");
                __admitted();
              },
              "");
          __ghost(
              [&]() {
                __consumes(
                    "for c in 0..3 -> for i in 0..n -> &D[MINDEX2(n, 3, i, c)] "
                    "~> Cell");
                __produces("for i in 0..n -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
                __produces("for i in 0..n -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
                __produces("for i in 0..n -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
                __admitted();
                __with("justif := unroll");
              },
              "");
          __ghost(assume, "F := in_range(1, 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
          __ghost(
              [&]() {
                __consumes("for i in 0..1 -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
                __produces("&D[MINDEX2(n, 3, 0, 0)] ~> Cell");
                __admitted();
                __with("justif := unroll");
              },
              "");
          __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
          uint16_t s = (uint16_t)0;
          __ghost(to_prove, "F := __is_neq(3, 0)");
          __ghost(group_shift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 0)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 0)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "0)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(to_prove, "F := __is_neq(3, 0)");
          __ghost(group_scale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 0)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 0)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(assume, "F := in_range(1, 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
          __ghost(
              [&]() {
                __consumes("for i in 0..1 -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
                __produces("&D[MINDEX2(n, 3, 0, 1)] ~> Cell");
                __admitted();
                __with("justif := unroll");
              },
              "");
          __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
          uint16_t s2 = (uint16_t)0;
          __ghost(to_prove, "F := __is_neq(3, 0)");
          __ghost(group_shift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 1)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 1)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "1)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(to_prove, "F := __is_neq(3, 0)");
          __ghost(group_scale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 1)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 1)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(assume, "F := in_range(1, 0..n)");
          __ghost(group_split,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
          __ghost(
              [&]() {
                __consumes("for i in 0..1 -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
                __produces("&D[MINDEX2(n, 3, 0, 2)] ~> Cell");
                __admitted();
                __with("justif := unroll");
              },
              "");
          __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
          uint16_t s3 = (uint16_t)0;
          __ghost(to_prove, "F := __is_neq(3, 0)");
          for (int i = 0; i < 3 * kn; i += 3) {
            __strict();
            __smodifies("&s ~> Cell");
            __smodifies("&s2 ~> Cell");
            __smodifies("&s3 ~> Cell");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_9 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 0, m := n + "
                              "kn - 1, n := 3");
            s = s + (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 0)];
            __ghost_end(__ghost_pair_9);
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_928 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 1, m := n + "
                              "kn - 1, n := 3");
            s2 = s2 + (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 1)];
            __ghost_end(__ghost_pair_928);
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_924 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 2, m := n + "
                              "kn - 1, n := 3");
            s3 = s3 + (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 2)];
            __ghost_end(__ghost_pair_924);
          }
          D[MINDEX2(n, 3, 0, 0)] = s;
          __ghost(
              [&]() {
                __consumes("&D[MINDEX2(n, 3, 0, 0)] ~> Cell");
                __produces("for i in 0..1 -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
                __admitted();
                __with("justif := roll");
              },
              "");
          D[MINDEX2(n, 3, 0, 1)] = s2;
          __ghost(
              [&]() {
                __consumes("&D[MINDEX2(n, 3, 0, 1)] ~> Cell");
                __produces("for i in 0..1 -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
                __admitted();
                __with("justif := roll");
              },
              "");
          D[MINDEX2(n, 3, 0, 2)] = s3;
          __ghost(
              [&]() {
                __consumes("&D[MINDEX2(n, 3, 0, 2)] ~> Cell");
                __produces("for i in 0..1 -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
                __admitted();
                __with("justif := roll");
              },
              "");
          __ghost(group_shift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 2)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 2)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "2)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(to_prove, "F := __is_neq(3, 0)");
          __ghost(group_scale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 2)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 2)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          for (int i = 0; i < 3 * (n - 1); i += 3) {
            __strict();
            __smodifies("&s ~> Cell");
            __smodifies("&s2 ~> Cell");
            __smodifies("&s3 ~> Cell");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __xmodifies("&D[MINDEX2(n, 3, exact_div(i, 3) + 1, 0)] ~> Cell");
            __xmodifies("&D[MINDEX2(n, 3, exact_div(i, 3) + 1, 2)] ~> Cell");
            __xmodifies("&D[MINDEX2(n, 3, exact_div(i, 3) + 1, 1)] ~> Cell");
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..(n - 1))");
            __ghost(assume, "F := in_range(exact_div(i, 3) + 1, 1..n)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + kn..exact_div(i, 3) + "
                    "1 + kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3)..exact_div(i, 3) + 1, "
                    "0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + 1..exact_div(i, 3) + 1 "
                    "+ kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3) + kn, exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3) + kn, r1 := exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_11 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3) + kn, j := 0, m := "
                              "n + kn - 1, n := 3");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3), exact_div(i, "
                    "3)..exact_div(i, 3) + 1)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := exact_div(i, 3)..exact_div(i, "
                    "3) + 1, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_12 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 0, m := n + "
                              "kn - 1, n := 3");
            s = s +
                (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3) + kn, 0)] -
                (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 0)];
            __ghost_end(__ghost_pair_12);
            __ghost_end(__ghost_pair_11);
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..(n - 1))");
            __ghost(assume, "F := in_range(exact_div(i, 3) + 1, 1..n)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + kn..exact_div(i, 3) + "
                    "1 + kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3)..exact_div(i, 3) + 1, "
                    "0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + 1..exact_div(i, 3) + 1 "
                    "+ kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3) + kn, exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3) + kn, r1 := exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1119 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3) + kn, j := 1, m := "
                              "n + kn - 1, n := 3");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3), exact_div(i, "
                    "3)..exact_div(i, 3) + 1)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := exact_div(i, 3)..exact_div(i, "
                    "3) + 1, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1220 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 1, m := n + "
                              "kn - 1, n := 3");
            s2 = s2 +
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3) + kn, 1)] -
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 1)];
            __ghost_end(__ghost_pair_1220);
            __ghost_end(__ghost_pair_1119);
            __ghost(assume, "F := in_range(exact_div(i, 3), 0..(n - 1))");
            __ghost(assume, "F := in_range(exact_div(i, 3) + 1, 1..n)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + kn..exact_div(i, 3) + "
                    "1 + kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3)..exact_div(i, 3) + 1, "
                    "0..n + kn - 1)");
            __ghost(assume,
                    "F := is_subrange(exact_div(i, 3) + 1..exact_div(i, 3) + 1 "
                    "+ kn, 0..n + kn - 1)");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3) + kn, exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3) + kn, r1 := exact_div(i, 3) + "
                    "kn..exact_div(i, 3) + 1 + kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1112 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3) + kn, j := 2, m := "
                              "n + kn - 1, n := 3");
            __ghost(assume,
                    "F := in_range(exact_div(i, 3), exact_div(i, "
                    "3)..exact_div(i, 3) + 1)");
            __ghost(in_range_extend,
                    "x := exact_div(i, 3), r1 := exact_div(i, 3)..exact_div(i, "
                    "3) + 1, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1213 =
                __ghost_begin(matrix2_ro_focus,
                              "M := S, i := exact_div(i, 3), j := 2, m := n + "
                              "kn - 1, n := 3");
            s3 = s3 +
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3) + kn, 2)] -
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, exact_div(i, 3), 2)];
            __ghost_end(__ghost_pair_1213);
            __ghost_end(__ghost_pair_1112);
            D[MINDEX2(n, 3, exact_div(i, 3) + 1, 0)] = s;
            D[MINDEX2(n, 3, exact_div(i, 3) + 1, 1)] = s2;
            D[MINDEX2(n, 3, exact_div(i, 3) + 1, 2)] = s3;
          }
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 0)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unscale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 0)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "0)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 0)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unshift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 0)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 1)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unscale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 1)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "1)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 1)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unshift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 1)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
          __ghost(
              [&]() {
                __modifies(
                    "for TMP_2 in range(0, 3 * (n - 1), 3) -> &D[MINDEX2(n, "
                    "3, exact_div(TMP_2, 3) + 1, 2)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unscale,
                  "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                  "3, i + 1, 2)] ~> Cell, factor := 3, new_step := 3, new_stop "
                  ":= 3 * (n - 1)");
          __ghost(
              [&]() {
                __consumes(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 + 1, "
                    "2)] ~> Cell");
                __produces(
                    "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 3, TMP_1 - (-"
                    "1), 2)] ~> Cell");
                __admitted();
                __with("justif := arith_simpl");
              },
              "");
          __ghost(group_unshift,
                  "start := 1, stop := n, step := 1, items := fun i -> "
                  "&D[MINDEX2(n, 3, i, 2)] ~> Cell, shift := - 1, new_start := "
                  "0, new_stop := n - 1");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
          __ghost(
              [&]() {
                __consumes("for i in 0..n -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
                __consumes("for i in 0..n -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
                __consumes("for i in 0..n -> &D[MINDEX2(n, 3, i, 2)] ~> Cell");
                __produces(
                    "for c in 0..3 -> for i in 0..n -> &D[MINDEX2(n, 3, i, c)] "
                    "~> Cell");
                __admitted();
                __with("justif := roll");
              },
              "");
          __ghost(
              [&]() {
                __requires("__is_geq(3, 0)");
                __requires("__not(__is_eq(3, 1))");
                __requires("__is_eq(3, 3)");
                __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, 3))");
                __consumes(
                    "for c in 0..3 -> for i in 0..n -> &D[MINDEX2(n, 3, i, c)] "
                    "~> Cell");
                __ensures("__is_geq(cn, 0)");
                __ensures("__not(__is_eq(cn, 1))");
                __ensures("__is_eq(cn, 3)");
                __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
                __produces(
                    "for c in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, "
                    "c)] ~> Cell");
                __admitted();
              },
              "");
        } /*cn@*/
        else {
          if (cn == 4) /*@cn*/ {
            __ghost(assert_alias, "x := cn, y := 4");
            __ghost(
                [&]() {
                  __requires("__is_geq(cn, 0)");
                  __requires("__not(__is_eq(cn, 1))");
                  __requires("__not(__is_eq(cn, 3))");
                  __requires("__is_eq(cn, 4)");
                  __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
                  __consumes(
                      "for j in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, "
                      "j)] ~> Cell");
                  __ensures("__is_geq(4, 0)");
                  __ensures("__not(__is_eq(4, 1))");
                  __ensures("__not(__is_eq(4, 3))");
                  __ensures("__is_eq(4, 4)");
                  __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, 4))");
                  __produces(
                      "for j in 0..4 -> for i in 0..n -> &D[MINDEX2(n, 4, i, "
                      "j)] ~> Cell");
                  __admitted();
                },
                "");
            __ghost(
                [&]() {
                  __consumes(
                      "for c in 0..4 -> for i in 0..n -> &D[MINDEX2(n, 4, i, "
                      "c)] ~> Cell");
                  __produces(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
                  __produces(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
                  __produces(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
                  __produces(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
                  __admitted();
                  __with("justif := unroll");
                },
                "");
            __ghost(assume, "F := in_range(1, 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
            __ghost(
                [&]() {
                  __consumes(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
                  __produces("&D[MINDEX2(n, 4, 0, 0)] ~> Cell");
                  __admitted();
                  __with("justif := unroll");
                },
                "");
            __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
            uint16_t s = (uint16_t)0;
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_shift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 0)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 0)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 0)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_scale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 0)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 0)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(assume, "F := in_range(1, 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
            __ghost(
                [&]() {
                  __consumes(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
                  __produces("&D[MINDEX2(n, 4, 0, 1)] ~> Cell");
                  __admitted();
                  __with("justif := unroll");
                },
                "");
            __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
            uint16_t s4 = (uint16_t)0;
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_shift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 1)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 1)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 1)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_scale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 1)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 1)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(assume, "F := in_range(1, 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
            __ghost(
                [&]() {
                  __consumes(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
                  __produces("&D[MINDEX2(n, 4, 0, 2)] ~> Cell");
                  __admitted();
                  __with("justif := unroll");
                },
                "");
            __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
            uint16_t s5 = (uint16_t)0;
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_shift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 2)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 2)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 2)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_scale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 2)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 2)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(assume, "F := in_range(1, 0..n)");
            __ghost(group_split,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
            __ghost(
                [&]() {
                  __consumes(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
                  __produces("&D[MINDEX2(n, 4, 0, 3)] ~> Cell");
                  __admitted();
                  __with("justif := unroll");
                },
                "");
            __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
            uint16_t s6 = (uint16_t)0;
            __ghost(to_prove, "F := __is_neq(4, 0)");
            for (int i = 0; i < 4 * kn; i += 4) {
              __strict();
              __smodifies("&s ~> Cell");
              __smodifies("&s4 ~> Cell");
              __smodifies("&s5 ~> Cell");
              __smodifies("&s6 ~> Cell");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := 0..kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_9 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 0, m := n "
                                "+ kn - 1, n := 4");
              s = s + (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 0)];
              __ghost_end(__ghost_pair_9);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := 0..kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_961 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 1, m := n "
                                "+ kn - 1, n := 4");
              s4 = s4 + (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 1)];
              __ghost_end(__ghost_pair_961);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := 0..kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_957 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 2, m := n "
                                "+ kn - 1, n := 4");
              s5 = s5 + (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 2)];
              __ghost_end(__ghost_pair_957);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := 0..kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_953 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 3, m := n "
                                "+ kn - 1, n := 4");
              s6 = s6 + (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 3)];
              __ghost_end(__ghost_pair_953);
            }
            D[MINDEX2(n, 4, 0, 0)] = s;
            __ghost(
                [&]() {
                  __consumes("&D[MINDEX2(n, 4, 0, 0)] ~> Cell");
                  __produces(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
                  __admitted();
                  __with("justif := roll");
                },
                "");
            D[MINDEX2(n, 4, 0, 1)] = s4;
            __ghost(
                [&]() {
                  __consumes("&D[MINDEX2(n, 4, 0, 1)] ~> Cell");
                  __produces(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
                  __admitted();
                  __with("justif := roll");
                },
                "");
            D[MINDEX2(n, 4, 0, 2)] = s5;
            __ghost(
                [&]() {
                  __consumes("&D[MINDEX2(n, 4, 0, 2)] ~> Cell");
                  __produces(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
                  __admitted();
                  __with("justif := roll");
                },
                "");
            D[MINDEX2(n, 4, 0, 3)] = s6;
            __ghost(
                [&]() {
                  __consumes("&D[MINDEX2(n, 4, 0, 3)] ~> Cell");
                  __produces(
                      "for i in 0..1 -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
                  __admitted();
                  __with("justif := roll");
                },
                "");
            __ghost(group_shift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 3)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0 -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 3)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0 -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 3)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(to_prove, "F := __is_neq(4, 0)");
            __ghost(group_scale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 3)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 3)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            for (int i = 0; i < 4 * (n - 1); i += 4) {
              __strict();
              __smodifies("&s ~> Cell");
              __smodifies("&s4 ~> Cell");
              __smodifies("&s5 ~> Cell");
              __smodifies("&s6 ~> Cell");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __sreads("S ~> Matrix2(n + kn - 1, 4)");
              __xmodifies("&D[MINDEX2(n, 4, exact_div(i, 4) + 1, 0)] ~> Cell");
              __xmodifies("&D[MINDEX2(n, 4, exact_div(i, 4) + 1, 2)] ~> Cell");
              __xmodifies("&D[MINDEX2(n, 4, exact_div(i, 4) + 1, 3)] ~> Cell");
              __xmodifies("&D[MINDEX2(n, 4, exact_div(i, 4) + 1, 1)] ~> Cell");
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..(n - 1))");
              __ghost(assume, "F := in_range(exact_div(i, 4) + 1, 1..n)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + kn..exact_div(i, 4) "
                      "+ 1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4)..exact_div(i, 4) + 1, "
                      "0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + 1..exact_div(i, 4) + "
                      "1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4) + kn, exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4) + kn, r1 := exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_11 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4) + kn, j := 0, m "
                                ":= n + kn - 1, n := 4");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4), exact_div(i, "
                      "4)..exact_div(i, 4) + 1)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := exact_div(i, "
                      "4)..exact_div(i, 4) + 1, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_12 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 0, m := n "
                                "+ kn - 1, n := 4");
              s = s +
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4) + kn, 0)] -
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 0)];
              __ghost_end(__ghost_pair_12);
              __ghost_end(__ghost_pair_11);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..(n - 1))");
              __ghost(assume, "F := in_range(exact_div(i, 4) + 1, 1..n)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + kn..exact_div(i, 4) "
                      "+ 1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4)..exact_div(i, 4) + 1, "
                      "0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + 1..exact_div(i, 4) + "
                      "1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4) + kn, exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4) + kn, r1 := exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1148 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4) + kn, j := 1, m "
                                ":= n + kn - 1, n := 4");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4), exact_div(i, "
                      "4)..exact_div(i, 4) + 1)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := exact_div(i, "
                      "4)..exact_div(i, 4) + 1, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1249 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 1, m := n "
                                "+ kn - 1, n := 4");
              s4 =
                  s4 +
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4) + kn, 1)] -
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 1)];
              __ghost_end(__ghost_pair_1249);
              __ghost_end(__ghost_pair_1148);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..(n - 1))");
              __ghost(assume, "F := in_range(exact_div(i, 4) + 1, 1..n)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + kn..exact_div(i, 4) "
                      "+ 1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4)..exact_div(i, 4) + 1, "
                      "0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + 1..exact_div(i, 4) + "
                      "1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4) + kn, exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4) + kn, r1 := exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1141 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4) + kn, j := 2, m "
                                ":= n + kn - 1, n := 4");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4), exact_div(i, "
                      "4)..exact_div(i, 4) + 1)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := exact_div(i, "
                      "4)..exact_div(i, 4) + 1, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1242 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 2, m := n "
                                "+ kn - 1, n := 4");
              s5 =
                  s5 +
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4) + kn, 2)] -
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 2)];
              __ghost_end(__ghost_pair_1242);
              __ghost_end(__ghost_pair_1141);
              __ghost(assume, "F := in_range(exact_div(i, 4), 0..(n - 1))");
              __ghost(assume, "F := in_range(exact_div(i, 4) + 1, 1..n)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + kn..exact_div(i, 4) "
                      "+ 1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4)..exact_div(i, 4) + 1, "
                      "0..n + kn - 1)");
              __ghost(assume,
                      "F := is_subrange(exact_div(i, 4) + 1..exact_div(i, 4) + "
                      "1 + kn, 0..n + kn - 1)");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4) + kn, exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4) + kn, r1 := exact_div(i, 4) + "
                      "kn..exact_div(i, 4) + 1 + kn, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1134 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4) + kn, j := 3, m "
                                ":= n + kn - 1, n := 4");
              __ghost(assume,
                      "F := in_range(exact_div(i, 4), exact_div(i, "
                      "4)..exact_div(i, 4) + 1)");
              __ghost(in_range_extend,
                      "x := exact_div(i, 4), r1 := exact_div(i, "
                      "4)..exact_div(i, 4) + 1, r2 := 0..n + kn - 1");
              const __ghost_fn __ghost_pair_1235 =
                  __ghost_begin(matrix2_ro_focus,
                                "M := S, i := exact_div(i, 4), j := 3, m := n "
                                "+ kn - 1, n := 4");
              s6 =
                  s6 +
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4) + kn, 3)] -
                  (uint16_t)S[MINDEX2(n + kn - 1, 4, exact_div(i, 4), 3)];
              __ghost_end(__ghost_pair_1235);
              __ghost_end(__ghost_pair_1134);
              D[MINDEX2(n, 4, exact_div(i, 4) + 1, 0)] = s;
              D[MINDEX2(n, 4, exact_div(i, 4) + 1, 1)] = s4;
              D[MINDEX2(n, 4, exact_div(i, 4) + 1, 2)] = s5;
              D[MINDEX2(n, 4, exact_div(i, 4) + 1, 3)] = s6;
            }
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 0)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unscale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 0)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 0)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 0)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unshift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 0)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 1)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unscale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 1)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 1)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 1)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unshift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 1)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 2)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unscale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 2)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 2)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 2)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unshift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 2)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
            __ghost(
                [&]() {
                  __modifies(
                      "for TMP_2 in range(0, 4 * (n - 1), 4) -> "
                      "&D[MINDEX2(n, 4, exact_div(TMP_2, 4) + 1, 3)] ~> "
                      "Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unscale,
                    "stop := n - 1, step := 1, items := fun i -> &D[MINDEX2(n, "
                    "4, i + 1, 3)] ~> Cell, factor := 4, new_step := 4, "
                    "new_stop := 4 * (n - 1)");
            __ghost(
                [&]() {
                  __consumes(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 + "
                      "1, 3)] ~> Cell");
                  __produces(
                      "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, 4, TMP_1 - (-"
                      "1), 3)] ~> Cell");
                  __admitted();
                  __with("justif := arith_simpl");
                },
                "");
            __ghost(group_unshift,
                    "start := 1, stop := n, step := 1, items := fun i -> "
                    "&D[MINDEX2(n, 4, i, 3)] ~> Cell, shift := - 1, new_start "
                    ":= 0, new_stop := n - 1");
            __ghost(group_join,
                    "start := 0, stop := n, step := 1, split := 1, items := "
                    "fun i -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
            __ghost(
                [&]() {
                  __consumes(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 0)] ~> Cell");
                  __consumes(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 1)] ~> Cell");
                  __consumes(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 2)] ~> Cell");
                  __consumes(
                      "for i in 0..n -> &D[MINDEX2(n, 4, i, 3)] ~> Cell");
                  __produces(
                      "for c in 0..4 -> for i in 0..n -> &D[MINDEX2(n, 4, i, "
                      "c)] ~> Cell");
                  __admitted();
                  __with("justif := roll");
                },
                "");
            __ghost(
                [&]() {
                  __requires("__is_geq(4, 0)");
                  __requires("__not(__is_eq(4, 1))");
                  __requires("__not(__is_eq(4, 3))");
                  __requires("__is_eq(4, 4)");
                  __consumes("_RO(#_4, S ~> Matrix2(n + kn - 1, 4))");
                  __consumes(
                      "for c in 0..4 -> for i in 0..n -> &D[MINDEX2(n, 4, i, "
                      "c)] ~> Cell");
                  __ensures("__is_geq(cn, 0)");
                  __ensures("__not(__is_eq(cn, 1))");
                  __ensures("__not(__is_eq(cn, 3))");
                  __ensures("__is_eq(cn, 4)");
                  __produces("_RO(#_4, S ~> Matrix2(n + kn - 1, cn))");
                  __produces(
                      "for c in 0..cn -> for i in 0..n -> &D[MINDEX2(n, cn, i, "
                      "c)] ~> Cell");
                  __admitted();
                },
                "");
          } /*cn@*/
          else /*@generic*/ {
            for (int c = 0; c < cn; c++) {
              __strict();
              __sreads("S ~> Matrix2(n + kn - 1, cn)");
              __xmodifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
              __ghost(assume, "F := in_range(1, 0..n)");
              __ghost(group_split,
                      "start := 0, stop := n, step := 1, split := 1, items := "
                      "fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
              __ghost(
                  [&]() {
                    __consumes(
                        "for i in 0..1 -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
                    __produces("&D[MINDEX2(n, cn, 0, c)] ~> Cell");
                    __admitted();
                    __with("justif := unroll");
                  },
                  "");
              __ghost(assume, "F := is_subrange(0..kn, 0..n + kn - 1)");
              uint16_t s = (uint16_t)0;
              __ghost(to_prove, "F := __is_neq(cn, 0)");
              for (int i = 0; i < cn * kn; i += cn) {
                __strict();
                __smodifies("&s ~> Cell");
                __sreads("S ~> Matrix2(n + kn - 1, cn)");
                __ghost(assume, "F := in_range(exact_div(i, cn), 0..kn)");
                __ghost(assume, "F := in_range(exact_div(i, cn), 0..kn)");
                __ghost(
                    in_range_extend,
                    "x := exact_div(i, cn), r1 := 0..kn, r2 := 0..n + kn - 1");
                const __ghost_fn __ghost_pair_9 =
                    __ghost_begin(matrix2_ro_focus,
                                  "M := S, i := exact_div(i, cn), j := c, m := "
                                  "n + kn - 1, n := cn");
                s = s +
                    (uint16_t)S[MINDEX2(n + kn - 1, cn, exact_div(i, cn), c)];
                __ghost_end(__ghost_pair_9);
              }
              D[MINDEX2(n, cn, 0, c)] = s;
              __ghost(
                  [&]() {
                    __consumes("&D[MINDEX2(n, cn, 0, c)] ~> Cell");
                    __produces(
                        "for i in 0..1 -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
                    __admitted();
                    __with("justif := roll");
                  },
                  "");
              __ghost(group_shift,
                      "start := 1, stop := n, step := 1, items := fun i -> "
                      "&D[MINDEX2(n, cn, i, c)] ~> Cell, shift := - 1, "
                      "new_start := 0, new_stop := n - 1");
              __ghost(
                  [&]() {
                    __consumes(
                        "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, cn, TMP_1 "
                        "- (-1), c)] ~> Cell");
                    __produces(
                        "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, cn, TMP_1 "
                        "+ 1, c)] ~> Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              __ghost(to_prove, "F := __is_neq(cn, 0)");
              __ghost(group_scale,
                      "stop := n - 1, step := 1, items := fun i -> "
                      "&D[MINDEX2(n, cn, i + 1, c)] ~> Cell, factor := cn, "
                      "new_step := cn, new_stop := cn * (n - 1)");
              __ghost(
                  [&]() {
                    __modifies(
                        "for TMP_2 in range(0, cn * (n - 1), cn) -> "
                        "&D[MINDEX2(n, cn, exact_div(TMP_2, cn) + 1, c)] ~> "
                        "Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              __ghost(group_shift,
                      "start := 0, stop := cn * (n - 1), step := cn, items := "
                      "fun i -> &D[MINDEX2(n, cn, exact_div(i, cn) + 1, c)] ~> "
                      "Cell, shift := c, new_start := 0 + c, new_stop := cn * "
                      "(n - 1) + c");
              __ghost(
                  [&]() {
                    __consumes(
                        "for TMP_3 in range(0 + c, cn * (n - 1) + c, cn) -> "
                        "&D[MINDEX2(n, cn, exact_div((TMP_3 - c), cn) + 1, "
                        "c)] ~> Cell");
                    __produces(
                        "for TMP_3 in range(c, cn * (n - 1) + c, cn) -> "
                        "&D[MINDEX2(n, cn, exact_div((TMP_3 - c), cn) + 1, "
                        "c)] ~> Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              for (int i = c; i < cn * (n - 1) + c; i += cn) {
                __strict();
                __smodifies("&s ~> Cell");
                __sreads("S ~> Matrix2(n + kn - 1, cn)");
                __xmodifies(
                    "&D[MINDEX2(n, cn, exact_div((i - c), cn) + 1, c)] ~> "
                    "Cell");
                __ghost(assume,
                        "F := in_range(i - c, range(0, cn * (n - 1), cn))");
                __ghost(assume,
                        "F := in_range(exact_div((i - c), cn), 0..(n - 1))");
                __ghost(assume,
                        "F := in_range(exact_div((i - c), cn) + 1, 1..n)");
                __ghost(assume,
                        "F := is_subrange(exact_div((i - c), cn) + "
                        "kn..exact_div((i - c), cn) + 1 + kn, 0..n + kn - 1)");
                __ghost(assume,
                        "F := is_subrange(exact_div((i - c), cn)..exact_div((i "
                        "- c), cn) + 1, 0..n + kn - 1)");
                __ghost(assume,
                        "F := is_subrange(exact_div((i - c), cn) + "
                        "1..exact_div((i - c), cn) + 1 + kn, 0..n + kn - 1)");
                __ghost(
                    assume,
                    "F := in_range(exact_div((i - c), cn) + kn, exact_div((i - "
                    "c), cn) + kn..exact_div((i - c), cn) + 1 + kn)");
                __ghost(in_range_extend,
                        "x := exact_div((i - c), cn) + kn, r1 := exact_div((i "
                        "- c), cn) + kn..exact_div((i - c), cn) + 1 + kn, r2 "
                        ":= 0..n + kn - 1");
                const __ghost_fn __ghost_pair_11 =
                    __ghost_begin(matrix2_ro_focus,
                                  "M := S, i := exact_div((i - c), cn) + kn, j "
                                  ":= c, m := n + kn - 1, n := cn");
                __ghost(assume,
                        "F := in_range(exact_div((i - c), cn), exact_div((i - "
                        "c), cn)..exact_div((i - c), cn) + 1)");
                __ghost(in_range_extend,
                        "x := exact_div((i - c), cn), r1 := exact_div((i - c), "
                        "cn)..exact_div((i - c), cn) + 1, r2 := 0..n + kn - 1");
                const __ghost_fn __ghost_pair_12 =
                    __ghost_begin(matrix2_ro_focus,
                                  "M := S, i := exact_div((i - c), cn), j := "
                                  "c, m := n + kn - 1, n := cn");
                s = s +
                    (uint16_t)S[MINDEX2(n + kn - 1, cn,
                                        exact_div((i - c), cn) + kn, c)] -
                    (uint16_t)
                        S[MINDEX2(n + kn - 1, cn, exact_div((i - c), cn), c)];
                __ghost_end(__ghost_pair_12);
                __ghost_end(__ghost_pair_11);
                D[MINDEX2(n, cn, exact_div((i - c), cn) + 1, c)] = s;
              }
              __ghost(
                  [&]() {
                    __consumes(
                        "for TMP_3 in range(c, cn * (n - 1) + c, cn) -> "
                        "&D[MINDEX2(n, cn, exact_div((TMP_3 - c), cn) + 1, "
                        "c)] ~> Cell");
                    __produces(
                        "for TMP_3 in range(0 + c, cn * (n - 1) + c, cn) -> "
                        "&D[MINDEX2(n, cn, exact_div((TMP_3 - c), cn) + 1, "
                        "c)] ~> Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              __ghost(group_unshift,
                      "start := 0, stop := cn * (n - 1), step := cn, items := "
                      "fun i -> &D[MINDEX2(n, cn, exact_div(i, cn) + 1, c)] ~> "
                      "Cell, shift := c, new_start := 0 + c, new_stop := cn * "
                      "(n - 1) + c");
              __ghost(
                  [&]() {
                    __modifies(
                        "for TMP_2 in range(0, cn * (n - 1), cn) -> "
                        "&D[MINDEX2(n, cn, exact_div(TMP_2, cn) + 1, c)] ~> "
                        "Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              __ghost(group_unscale,
                      "stop := n - 1, step := 1, items := fun i -> "
                      "&D[MINDEX2(n, cn, i + 1, c)] ~> Cell, factor := cn, "
                      "new_step := cn, new_stop := cn * (n - 1)");
              __ghost(
                  [&]() {
                    __consumes(
                        "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, cn, TMP_1 "
                        "+ 1, c)] ~> Cell");
                    __produces(
                        "for TMP_1 in 0..(n - 1) -> &D[MINDEX2(n, cn, TMP_1 "
                        "- (-1), c)] ~> Cell");
                    __admitted();
                    __with("justif := arith_simpl");
                  },
                  "");
              __ghost(group_unshift,
                      "start := 1, stop := n, step := 1, items := fun i -> "
                      "&D[MINDEX2(n, cn, i, c)] ~> Cell, shift := - 1, "
                      "new_start := 0, new_stop := n - 1");
              __ghost(group_join,
                      "start := 0, stop := n, step := 1, split := 1, items := "
                      "fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
            }
          } /*generic@*/
        }
      }
      __ghost(swap_groups,
              "outer_range := 0..cn, inner_range := 0..n, items := fun c, i -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
    } /*nokn@*/
  }
}
