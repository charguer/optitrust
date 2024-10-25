#include <optitrust.h>

void rowSum(const int kn, const uint8_t* S, uint16_t* D, const int n,
            const int cn) {
  __requires("__is_geq(kn, 0)");
  __requires("__is_geq(n, 1)");
  __requires("__is_geq(cn, 0)");
  __modifies("D ~> Matrix2(n, cn)");
  __requires("#4: _Fraction");
  __consumes("_RO(#4, S ~> Matrix2(n + kn - 1, cn))");
  __produces("_RO(#4, S ~> Matrix2(n + kn - 1, cn))");

      __ghost(swap_groups,
              "outer_range := 0..n, inner_range := 0..cn, items := fun i, c -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
      for (int c = 0; c < cn; c++) {
        __strict();
        __sreads("S ~> Matrix2(n + kn - 1, cn)");
        __xmodifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
        __ghost(assume, "F := in_range(1, 0..n)");
        __ghost(group_split,
                "start := 0, stop := n, step := 1, split := 1, items := fun i "
                "-> &D[MINDEX2(n, cn, i, c)] ~> Cell");
        __ghost(
            [&]() {
              __consumes("for i in 0..1 -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
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
          __ghost(in_range_extend, "x := i, r1 := 0..kn, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_9 =
              __ghost_begin(matrix2_ro_focus,
                            "M := S, i := i, j := c, m := n + kn - 1, n := cn");
          s = s + (uint16_t)S[MINDEX2(n + kn - 1, cn, i, c)];
          __ghost_end(__ghost_pair_9);
        }
        D[MINDEX2(n, cn, 0, c)] = s;
        __ghost(
            [&]() {
              __consumes("&D[MINDEX2(n, cn, 0, c)] ~> Cell");
              __produces("for i in 0..1 -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
              __admitted();
              __with("justif := roll");
            },
            "");
        /*@__547*/ for (int i = 1; i < n; i++) {
          __strict();
          __smodifies("&s ~> Cell");
          __sreads("S ~> Matrix2(n + kn - 1, cn)");
          __xmodifies("&D[MINDEX2(n, cn, i, c)] ~> Cell");
          __ghost(assume,
                  "F := is_subrange(i + kn - 1..i + kn, 0..n + kn - 1)");
          __ghost(assume, "F := is_subrange(i - 1..i, 0..n + kn - 1)");
          __ghost(assume, "F := is_subrange(i..i + kn, 0..n + kn - 1)");
          __ghost(
              in_range_extend,
              "x := i + kn - 1, r1 := i + kn - 1..i + kn, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_11 = __ghost_begin(
              matrix2_ro_focus,
              "M := S, i := i + kn - 1, j := c, m := n + kn - 1, n := cn");
          __ghost(in_range_extend,
                  "x := i - 1, r1 := i - 1..i, r2 := 0..n + kn - 1");
          const __ghost_fn __ghost_pair_12 = __ghost_begin(
              matrix2_ro_focus,
              "M := S, i := i - 1, j := c, m := n + kn - 1, n := cn");
          s = s + (uint16_t)S[MINDEX2(n + kn - 1, cn, i + kn - 1, c)] -
              (uint16_t)S[MINDEX2(n + kn - 1, cn, i - 1, c)];
          __ghost_end(__ghost_pair_12);
          __ghost_end(__ghost_pair_11);
          D[MINDEX2(n, cn, i, c)] = s;
        } /*__547@*/
        __ghost(group_join,
                "start := 0, stop := n, step := 1, split := 1, items := fun i "
                "-> &D[MINDEX2(n, cn, i, c)] ~> Cell");
      }
      __ghost(swap_groups,
              "outer_range := 0..cn, inner_range := 0..n, items := fun c, i -> "
              "&D[MINDEX2(n, cn, i, c)] ~> Cell");
}
