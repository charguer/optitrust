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
  __reads("S ~> Matrix2(n+kn, cn)");
  __modifies("D ~> Matrix2(n, cn)");

  for (int i = 0; i < n; i++) {
    __xmodifies("for c in 0..cn -> &D[MINDEX2(n, cn, i, c)] ~> Cell");

    for (int c = 0; c < cn; c++) {
      __xmodifies("&D[MINDEX2(n, cn, i, c)] ~> Cell");

      __ghost(assume, "is_subrange(i..i + kn, 0..n + kn)");
      // __GHOST_BEGIN(dfc, group2_ro_focus, "i := c, items := fun i, c -> &S[MINDEX2(n+kn, cn, i, c)] ~> Cell");
      // __GHOST_BEGIN(dfi, group_focus_subrange_ro, "i..i+kn, 0..n+kn");
      D[MINDEX2(n, cn, i, c)] = reduce_spe1(i, i+kn, S, n+kn, cn, c);
      /* reduce_add(i, i+kn, [&](int k) {
        (ST) S[MINDEX2(n+kn, cn, k, c)]
      }); */
      // __GHOST_END(dfi);
      // __GHOST_END(dfc);
    }
  }
}
