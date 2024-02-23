#include <optitrust.h>

typedef uint8_t T;
typedef uint16_t ST;

/*
  cn: number of (color) channels
  kn: size of box filter (convolution window)
  n: size of the row resulting from filtering
*/
void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  __reads("S ~> Matrix2(n+kn, cn)");
  __modifies("D ~> Matrix2(n, cn)"); // TODO: writes?

  __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
  for (int c = 0; c < cn; c++) { // foreach channel
    __parallel_reads("S ~> Matrix2(n+kn, cn)");
    __modifies("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell)");

    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < kn; i++) {
      __parallel_reads("S ~> Matrix2(n+kn, cn)");
      __sequentially_modifies("&s ~> Cell");
      __GHOST_BEGIN(sf, matrix2_ro_focus, "M := S, i := i, j := c");
      s += (ST) S[MINDEX2(n+kn, cn, i, c)];
      __GHOST_END(sf);
    }
    __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked,  bound_check_step := checked");
    D[MINDEX2(n, cn, 0, c)] = s;
    __GHOST_END(df);
    // for each pixel, shift the sliding window
    for (int i = 0; i < n-1; i++) {
      __parallel_reads("S ~> Matrix2(n+kn, cn)");
      __sequentially_modifies("&s ~> Cell");
      __sequentially_modifies("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell)");

      __GHOST_BEGIN(sf1, matrix2_ro_focus, "M := S, i := i, j := c");
      s -= (ST) S[MINDEX2(n+kn, cn, i, c)];
      __GHOST_END(sf1);
      __GHOST_BEGIN(sf2, matrix2_ro_focus, "M := S, i := i + kn, j := c");
      s += (ST) S[MINDEX2(n+kn, cn, i + kn, c)];
      __GHOST_END(sf2);
      __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
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

void rowSum_cn1(const int kn, const T* S, ST* D, const int n) {
  __reads("S ~> Matrix2(n+kn, 1)");
  __modifies("D ~> Matrix2(n, 1)"); // TODO: writes?

  rowSum(kn, S, D, n, 1);
}

void rowSum_cn3(const int kn, const T* S, ST* D, const int n) {
  __reads("S ~> Matrix2(n+kn, 3)");
  __modifies("D ~> Matrix2(n, 3)"); // TODO: writes?

  rowSum(kn, S, D, n, 3);
}

void rowSumOpt(const int kn, const T* S, ST* D, const int n, const int cn) {
  // introduce arbitrary conditions
  /* if (kn == 3) {
    // in this section, can do the substitution, or insert the line "const int kn = 3;".
    // ...
  } else if (kn == 5) {
    // ...
  } else */ if (cn == 1) {
    rowSum_cn1(kn, S, D, n);
  } else if (cn == 3) {
    rowSum_cn3(kn, S, D, n);
  } else {
    rowSum(kn, S, D, n, cn);
  }
}
