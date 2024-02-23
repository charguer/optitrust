#include <optitrust.h>

typedef uint8_t T;
typedef uint16_t ST;

/*
  cn: number of (color) channels
  ksize: size of box filter (convolution window)
  width: size of the row resulting from filtering
*/
void rowSum(const int ksize, const T* S, ST* D, const int width, const int cn) {
  __reads("S ~> Matrix2(width+ksize, cn)");
  __modifies("D ~> Matrix2(width, cn)"); // TODO: writes?

  __ghost(swap_groups, "items := fun i, k -> &D[MINDEX2(width, cn, i, k)] ~> Cell");
  for (int k = 0; k < cn; k++) { // foreach channel
    __parallel_reads("S ~> Matrix2(width+ksize, cn)");
    __modifies("Group(range(0, width, 1), fun i -> &D[MINDEX2(width, cn, i, k)] ~> Cell)");

    // initialize the sliding window
    ST s = 0;
    for (int i = 0; i < ksize; i++) {
      __parallel_reads("S ~> Matrix2(width+ksize, cn)");
      __sequentially_modifies("&s ~> Cell");
      __GHOST_BEGIN(sf, matrix2_ro_focus, "M := S, i := i, j := k");
      s += (ST) S[MINDEX2(width+ksize, cn, i, k)];
      __GHOST_END(sf);
    }
    __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(width, cn, i, k)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked,  bound_check_step := checked");
    D[MINDEX2(width, cn, 0, k)] = s;
    __GHOST_END(df);
    // for each pixel, shift the sliding window
    for (int i = 0; i < width-1; i++) {
      __parallel_reads("S ~> Matrix2(width+ksize, cn)");
      __sequentially_modifies("&s ~> Cell");
      __sequentially_modifies("Group(range(0, width, 1), fun i -> &D[MINDEX2(width, cn, i, k)] ~> Cell)");

      __GHOST_BEGIN(sf1, matrix2_ro_focus, "M := S, i := i, j := k");
      s -= (ST) S[MINDEX2(width+ksize, cn, i, k)];
      __GHOST_END(sf1);
      __GHOST_BEGIN(sf2, matrix2_ro_focus, "M := S, i := i + ksize, j := k");
      s += (ST) S[MINDEX2(width+ksize, cn, i + ksize, k)];
      __GHOST_END(sf2);
      __GHOST_BEGIN(df, group_focus, "items := fun i -> &D[MINDEX2(width, cn, i, k)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
      D[MINDEX2(width, cn, i + 1, k)] = s;
      __GHOST_END(df);
    }
    /* ALTERNATIVE
    for (int i = 1; i < width; i++) {
      s -= (ST) S[MINDEX2(?, cn, i - 1, k)];
      s += (ST) S[MINDEX2(?, cn, i + ksize-1, k)];
      D[MINDEX2(?, cn, i, k] = s;
    }
    */
  }
  __ghost(swap_groups_rev, "items := fun i, k -> &D[MINDEX2(width, cn, i, k)] ~> Cell");
}

void rowSum_cn1(const int ksize, const T* S, ST* D, const int width) {
  __reads("S ~> Matrix2(width+ksize, 1)");
  __modifies("D ~> Matrix2(width, 1)"); // TODO: writes?

  rowSum(ksize, S, D, width, 1);
}

void rowSum_cn3(const int ksize, const T* S, ST* D, const int width) {
  __reads("S ~> Matrix2(width+ksize, 3)");
  __modifies("D ~> Matrix2(width, 3)"); // TODO: writes?

  rowSum(ksize, S, D, width, 3);
}

void rowSumOpt(const int ksize, const T* S, ST* D, const int width, const int cn) {
  // introduce arbitrary conditions
  /* if (ksize == 3) {
    // in this section, can do the substitution, or insert the line "const int ksize = 3;".
    // ...
  } else if (ksize == 5) {
    // ...
  } else */ if (cn == 1) {
    rowSum_cn1(ksize, S, D, width);
  } else if (cn == 3) {
    rowSum_cn3(ksize, S, D, width);
  } else {
    rowSum(ksize, S, D, width, cn);
  }
}
