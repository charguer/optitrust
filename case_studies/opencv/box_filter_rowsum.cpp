#include <optitrust.h>

/*
  cn: number of (color) channels
  w: width of box filter (convolution window)
  n: size of the row resulting from filtering
*/
void rowSum(const int w, const uint8_t* S, uint16_t* D, const int n, const int cn) {
  __requires("w >= 0, n >= 1, cn >= 0");
  __reads("S ~> Matrix2(n+w-1, cn)");
  __writes("D ~> Matrix2(n, cn)");

  for (int i = 0; i < n; i++) { // for each pixel
    __sreads("S ~> Matrix2(n+w-1, cn)");
    __xwrites("for c in 0..cn -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

    for (int c = 0; c < cn; c++) { // foreach channel
      __sreads("S ~> Matrix2(n+w-1, cn)");
      __xwrites("&D[MINDEX2(n, cn, i, c)] ~> Cell");

      __ghost(assume, "is_subrange(i..i + w, 0..n + w - 1)"); // TODO: solve

      uint16_t s = (uint16_t)0;
      for (int k = i; k < i+w; k++) {
        __ghost(in_range_extend, "k, i..i+w, 0..n+w-1");
        __GHOST_BEGIN(focus, ro_matrix2_focus, "S, k, c");
        s += (uint16_t)S[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);
      }
      D[MINDEX2(n, cn, i, c)] = s;
    }
  }
}

/* explicit sliding window version:

void rowSum(const int w, const T* S, ST* D, const int n, const int cn) {
  __requires("w >= 0, n >= 1, cn >= 0");
  __requires("is_subrange(0..w, 0..(n + w))"); // TODO: solve
  __requires("is_subrange(0..(n-1), 0..(n + w))"); // TODO: solve
  __requires("is_subrange((0+w)..(n-1+w), 0..(n + w))"); // TODO: solve
  __requires("in_range(0, 0..n)"); // TODO: solve
  __reads("S ~> Matrix2(n+w, cn)");
  __modifies("D ~> Matrix2(n, cn)"); // TODO: writes?

  __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
  for (int c = 0; c < cn; c++) { // foreach channel
    __sreads("S ~> Matrix2(n+w, cn)");
    __xmodifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < w; i++) {
      __sreads("S ~> Matrix2(n+w, cn)");
      __smodifies("&s ~> Cell");

      __ghost(in_range_extend, "i, 0..w, 0..(n + w)");
      __GHOST_BEGIN(sf, ro_matrix2_focus, "M := S, i := i, j := c");
      s += (ST) S[MINDEX2(n+w, cn, i, c)];
      __GHOST_END(sf);
    }
    __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := 0");
    D[MINDEX2(n, cn, 0, c)] = s;
    __GHOST_END(df);
    // for each pixel, shift the sliding window
    for (int i = 0; i < n-1; i++) {
      __sreads("S ~> Matrix2(n+w, cn)");
      __smodifies("&s ~> Cell");
      __smodifies("for i in 0..n -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

      __ghost(in_range_extend, "i, 0..(n-1), 0..(n+w)");
      __GHOST_BEGIN(sf1, ro_matrix2_focus, "M := S, i := i, j := c");
      s -= (ST) S[MINDEX2(n+w, cn, i, c)];
      __GHOST_END(sf1);
      __ghost(in_range_shift_extend, "i, w, 0..(n+w), 0, n-1, 1");
      __GHOST_BEGIN(sf2, ro_matrix2_focus, "M := S, i := i + w, j := c");
      s += (ST) S[MINDEX2(n+w, cn, i + w, c)];
      __GHOST_END(sf2);
      __ghost(in_range_shift_extend, "i, 1, 0..n, 0, n-1, 1");
      __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := i + 1");
      D[MINDEX2(n, cn, i + 1, c)] = s;
      __GHOST_END(df);
    }
  }
  __ghost(swap_groups_rev, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
}
*/
