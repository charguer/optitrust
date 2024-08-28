#include <optitrust.h>

typedef uint8_t T;
typedef uint16_t ST;


void reduce_spe1_loop_out(uint16_t* out, int start, int stop, const uint8_t* in, int n, int m, int j) {
  __admitted();
  uint16_t s = 0;
  for (int i = start; i < stop; i++) {
    __ghost(assume, "in_range(j, 0..m)");
    __GHOST_BEGIN(focusA, matrix2_ro_focus, "in, i, j");
    s += (uint16_t) in[MINDEX2(n,m,i,j)];
    __GHOST_END(focusA);
  }
  *out = s;
}

void reduce_spe1_out(uint8_t* out, int start, int stop, const uint8_t* in, int n, int m, int j) {
  __admitted();
  *out = reduce_spe1(start, stop, in, n, m, j);
}


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

      ST s = 0;
      for (int j = i; j < i+kn; j++) { // for each source pixel
        __smodifies("&s ~> Cell");
        __ghost(assume, "in_range(j, 0..n + kn)");
        __GHOST_BEGIN(focusA, matrix2_ro_focus, "S, j, c");
        s += (ST) S[MINDEX2(n+kn,cn,j,c)];
        __GHOST_END(focusA);
      }
      D[MINDEX2(n,cn,i,c)] = s;



      //__ghost(assume, "is_subrange(i..i + kn, 0..n + kn)");
      //D[MINDEX2(n, cn, i, c)] = reduce_spe1(i, i+kn, S, n+kn, cn, c);

    }
  }
}
