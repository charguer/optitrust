#include <optitrust.h>

typedef uint8_t T;
typedef uint16_t ST;

/*
  cn: number of (color) channels
  kn: size of box filter (convolution window)
  n: size of the row resulting from filtering
*/
void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  __requires("kn >= 0, n >= 1, cn >= 0");
  __requires("is_subrange(0..kn, 0..(n + kn))"); // TODO: solve
  __requires("is_subrange(0..(n-1), 0..(n + kn))"); // TODO: solve
  __requires("is_subrange((0+kn)..(n-1+kn), 0..(n + kn))"); // TODO: solve
  __requires("in_range(0, 0..n)"); // TODO: solve
  __reads("S ~> Matrix2(n+kn, cn)");
  __modifies("D ~> Matrix2(n, cn)"); // TODO: writes?

  __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
  for (int c = 0; c < cn; c++) { // foreach channel
    __parallel_reads("S ~> Matrix2(n+kn, cn)");
    __modifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < kn; i++) {
      __parallel_reads("S ~> Matrix2(n+kn, cn)");
      __sequentially_modifies("&s ~> Cell");

      __ghost(in_range_extend, "i, 0..kn, 0..(n + kn)");
      __GHOST_BEGIN(sf, matrix2_ro_focus, "M := S, i := i, j := c");
      s += (ST) S[MINDEX2(n+kn, cn, i, c)];
      __GHOST_END(sf);
    }
    __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := 0");
    D[MINDEX2(n, cn, 0, c)] = s;
    __GHOST_END(df);
    // for each pixel, shift the sliding window
    for (int i = 0; i < n-1; i++) {
      __parallel_reads("S ~> Matrix2(n+kn, cn)");
      __sequentially_modifies("&s ~> Cell");
      __sequentially_modifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

      __ghost(in_range_extend, "i, 0..(n-1), 0..(n+kn)");
      __GHOST_BEGIN(sf1, matrix2_ro_focus, "M := S, i := i, j := c");
      s -= (ST) S[MINDEX2(n+kn, cn, i, c)];
      __GHOST_END(sf1);
      __ghost(in_range_shift_extend, "i, kn, 0..(n+kn), 0, n-1, 1");
      __GHOST_BEGIN(sf2, matrix2_ro_focus, "M := S, i := i + kn, j := c");
      s += (ST) S[MINDEX2(n+kn, cn, i + kn, c)];
      __GHOST_END(sf2);
      __ghost(in_range_shift_extend, "i, 1, 0..n, 0, n-1, 1");
      __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := i + 1");
      D[MINDEX2(n, cn, i + 1, c)] = s;
      __GHOST_END(df);
    }
    /* ALTERNATIVE
    for (int i = 1; i < n; i++) {
      s -= (ST) S[MINDEX2(?, cn, i - 1, c)];
      s += (ST) S[MINDEX2(?, cn, i + kn-1, c)];
      D[MINDEX2(?, cn, i, c] = s;
    }
    */
  }
  __ghost(swap_groups_rev, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
}
