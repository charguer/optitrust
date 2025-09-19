#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int kn, const uint8_t* S, uint16_t* D, const int n,
            const int cn) {
  __requires("__is_geq(kn, 0)");
  __requires("__is_geq(n, 1)");
  __requires("__is_geq(cn, 0)");
  __modifies("D ~> Matrix2(n, cn)");
  __reads("S ~> Matrix2(n + kn - 1, cn)");
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
      __ghost(in_range_extend,
              "x := ic / cn, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_3 = __ghost_begin(
          ro_matrix2_focus,
          "M := S, i := ic / cn, j := ic % cn, m := n + 2, n := cn");
      __ghost(in_range_extend,
              "x := ic / cn + 1, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_2 = __ghost_begin(
          ro_matrix2_focus,
          "M := S, i := ic / cn + 1, j := ic % cn, m := n + 2, n := cn");
      __ghost(in_range_extend,
              "x := ic / cn + 2, r1 := ic / cn..ic / cn + 3, r2 := 0..n + 2");
      const __ghost_fn __ghost_pair_1 = __ghost_begin(
          ro_matrix2_focus,
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
    else /*@nokn*/ {
      __ghost(swap_groups,
              "outer_range := 0..n, inner_range := 0..cn, items := fun i, c -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
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
          for (int i = 0; i < kn; i++) {
            __strict();
            __smodifies("&s ~> Cell");
            __smodifies("&s2 ~> Cell");
            __smodifies("&s3 ~> Cell");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __ghost(in_range_extend,
                    "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_9 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i, j := 0, m := n + kn - 1, n := 3");
            s = s + (uint16_t)S[MINDEX2(n + kn - 1, 3, i, 0)];
            __ghost_end(__ghost_pair_9);
            __ghost(in_range_extend,
                    "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_928 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i, j := 1, m := n + kn - 1, n := 3");
            s2 = s2 + (uint16_t)S[MINDEX2(n + kn - 1, 3, i, 1)];
            __ghost_end(__ghost_pair_928);
            __ghost(in_range_extend,
                    "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_924 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i, j := 2, m := n + kn - 1, n := 3");
            s3 = s3 + (uint16_t)S[MINDEX2(n + kn - 1, 3, i, 2)];
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
          for (int i = 1; i < n; i++) {
            __strict();
            __smodifies("&s ~> Cell");
            __smodifies("&s2 ~> Cell");
            __smodifies("&s3 ~> Cell");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __sreads("S ~> Matrix2(n + kn - 1, 3)");
            __xmodifies("&D[MINDEX2(n, 3, i, 0)] ~> Cell");
            __xmodifies("&D[MINDEX2(n, 3, i, 2)] ~> Cell");
            __xmodifies("&D[MINDEX2(n, 3, i, 1)] ~> Cell");
            __ghost(assume,
                    "F := is_subrange(i + kn - 1..i + kn, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i - 1..i, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i..i + kn, 0..n + kn - 1)");
            __ghost(in_range_extend,
                    "x := i + kn - 1, r1 := i + kn - 1..i + kn, r2 := 0..n + "
                    "kn - 1");
            const __ghost_fn __ghost_pair_11 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i + kn - 1, j := 0, m := n + kn - 1, n := 3");
            __ghost(in_range_extend,
                    "x := i - 1, r1 := i - 1..i, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_12 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i - 1, j := 0, m := n + kn - 1, n := 3");
            s = s + (uint16_t)S[MINDEX2(n + kn - 1, 3, i + kn - 1, 0)] -
                (uint16_t)S[MINDEX2(n + kn - 1, 3, i - 1, 0)];
            __ghost_end(__ghost_pair_12);
            __ghost_end(__ghost_pair_11);
            __ghost(assume,
                    "F := is_subrange(i + kn - 1..i + kn, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i - 1..i, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i..i + kn, 0..n + kn - 1)");
            __ghost(in_range_extend,
                    "x := i + kn - 1, r1 := i + kn - 1..i + kn, r2 := 0..n + "
                    "kn - 1");
            const __ghost_fn __ghost_pair_1119 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i + kn - 1, j := 1, m := n + kn - 1, n := 3");
            __ghost(in_range_extend,
                    "x := i - 1, r1 := i - 1..i, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1220 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i - 1, j := 1, m := n + kn - 1, n := 3");
            s2 = s2 + (uint16_t)S[MINDEX2(n + kn - 1, 3, i + kn - 1, 1)] -
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, i - 1, 1)];
            __ghost_end(__ghost_pair_1220);
            __ghost_end(__ghost_pair_1119);
            __ghost(assume,
                    "F := is_subrange(i + kn - 1..i + kn, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i - 1..i, 0..n + kn - 1)");
            __ghost(assume, "F := is_subrange(i..i + kn, 0..n + kn - 1)");
            __ghost(in_range_extend,
                    "x := i + kn - 1, r1 := i + kn - 1..i + kn, r2 := 0..n + "
                    "kn - 1");
            const __ghost_fn __ghost_pair_1112 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i + kn - 1, j := 2, m := n + kn - 1, n := 3");
            __ghost(in_range_extend,
                    "x := i - 1, r1 := i - 1..i, r2 := 0..n + kn - 1");
            const __ghost_fn __ghost_pair_1213 = __ghost_begin(
                ro_matrix2_focus,
                "M := S, i := i - 1, j := 2, m := n + kn - 1, n := 3");
            s3 = s3 + (uint16_t)S[MINDEX2(n + kn - 1, 3, i + kn - 1, 2)] -
                 (uint16_t)S[MINDEX2(n + kn - 1, 3, i - 1, 2)];
            __ghost_end(__ghost_pair_1213);
            __ghost_end(__ghost_pair_1112);
            D[MINDEX2(n, 3, i, 0)] = s;
            D[MINDEX2(n, 3, i, 1)] = s2;
            D[MINDEX2(n, 3, i, 2)] = s3;
          }
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell");
          __ghost(group_join,
                  "start := 0, stop := n, step := 1, split := 1, items := fun "
                  "i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell");
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
              for (int i = 0; i < kn; i++) {
                __strict();
                __smodifies("&s ~> Cell");
                __sreads("S ~> Matrix2(n + kn - 1, cn)");
                __ghost(in_range_extend,
                        "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
                const __ghost_fn __ghost_pair_9 = __ghost_begin(
                    ro_matrix2_focus,
                    "M := S, i := i, j := c, m := n + kn - 1, n := cn");
                s = s + (uint16_t)S[MINDEX2(n + kn - 1, cn, i, c)];
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
              for (int i = 1; i < n; i++) {
                __strict();
                __smodifies("&s ~> Cell");
                __sreads("S ~> Matrix2(n + kn - 1, cn)");
                __xmodifies("&D[MINDEX2(n, cn, i, c)] ~> Cell");
                __ghost(assume,
                        "F := is_subrange(i + kn - 1..i + kn, 0..n + kn - 1)");
                __ghost(assume, "F := is_subrange(i - 1..i, 0..n + kn - 1)");
                __ghost(assume, "F := is_subrange(i..i + kn, 0..n + kn - 1)");
                __ghost(in_range_extend,
                        "x := i + kn - 1, r1 := i + kn - 1..i + kn, r2 := 0..n "
                        "+ kn - 1");
                const __ghost_fn __ghost_pair_11 =
                    __ghost_begin(ro_matrix2_focus,
                                  "M := S, i := i + kn - 1, j := c, m := n + "
                                  "kn - 1, n := cn");
                __ghost(in_range_extend,
                        "x := i - 1, r1 := i - 1..i, r2 := 0..n + kn - 1");
                const __ghost_fn __ghost_pair_12 = __ghost_begin(
                    ro_matrix2_focus,
                    "M := S, i := i - 1, j := c, m := n + kn - 1, n := cn");
                s = s + (uint16_t)S[MINDEX2(n + kn - 1, cn, i + kn - 1, c)] -
                    (uint16_t)S[MINDEX2(n + kn - 1, cn, i - 1, c)];
                __ghost_end(__ghost_pair_12);
                __ghost_end(__ghost_pair_11);
                D[MINDEX2(n, cn, i, c)] = s;
              }
              __ghost(group_join,
                      "start := 0, stop := n, step := 1, split := 1, items := "
                      "fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
            }
          }
      __ghost(swap_groups,
              "outer_range := 0..cn, inner_range := 0..n, items := fun c, i -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
    } /*nokn@*/
}
