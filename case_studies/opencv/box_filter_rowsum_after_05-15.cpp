#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  if (kn == 3) /*@kn*/ {
    for (int ic = 0; ic < n * cn; ic++) {
      D[MINDEX2(n, cn, ic / cn, ic % cn)] =
          (uint16_t)S[MINDEX2(n + 3, cn, ic / cn, ic % cn)] +
          (uint16_t)S[MINDEX2(n + 3, cn, ic / cn + 1, ic % cn)] +
          (uint16_t)S[MINDEX2(n + 3, cn, ic / cn + 2, ic % cn)];
    }
  } /*kn@*/
  else {
    if (kn == 5) /*@kn*/ {
      for (int ic = 0; ic < n * cn; ic++) {
        D[MINDEX2(n, cn, ic / cn, ic % cn)] =
            (uint16_t)S[MINDEX2(n + 5, cn, ic / cn, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 5, cn, ic / cn + 1, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 5, cn, ic / cn + 2, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 5, cn, ic / cn + 3, ic % cn)] +
            (uint16_t)S[MINDEX2(n + 5, cn, ic / cn + 4, ic % cn)];
      }
    } /*kn@*/
    else {
      if (cn == 1) /*@cn*/ {
        uint16_t s = (uint16_t)0;
        for (int i = 0; i < kn; i++) {
          s = s + (uint16_t)S[MINDEX2(n + kn, 1, i, 0)];
        }
        D[MINDEX2(n, 1, 0, 0)] = s;
        for (int i = 1; i < n; i++) {
          s = s + (uint16_t)S[MINDEX2(n + kn, 1, i + kn - 1, 0)] -
              (uint16_t)S[MINDEX2(n + kn, 1, i - 1, 0)];
          D[MINDEX2(n, 1, i, 0)] = s;
        }
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          uint16_t s = (uint16_t)0;
          uint16_t s5 = (uint16_t)0;
          uint16_t s6 = (uint16_t)0;
          for (int i = 0; i < kn; i++) {
            s = s + (uint16_t)S[MINDEX2(n + kn, 3, i, 0)];
            s5 = s5 + (uint16_t)S[MINDEX2(n + kn, 3, i, 1)];
            s6 = s6 + (uint16_t)S[MINDEX2(n + kn, 3, i, 2)];
          }
          D[MINDEX2(n, 3, 0, 0)] = s;
          D[MINDEX2(n, 3, 0, 1)] = s5;
          D[MINDEX2(n, 3, 0, 2)] = s6;
          for (int i = 1; i < n; i++) {
            s = s + (uint16_t)S[MINDEX2(n + kn, 3, i + kn - 1, 0)] -
                (uint16_t)S[MINDEX2(n + kn, 3, i - 1, 0)];
            s5 = s5 + (uint16_t)S[MINDEX2(n + kn, 3, i + kn - 1, 1)] -
                 (uint16_t)S[MINDEX2(n + kn, 3, i - 1, 1)];
            s6 = s6 + (uint16_t)S[MINDEX2(n + kn, 3, i + kn - 1, 2)] -
                 (uint16_t)S[MINDEX2(n + kn, 3, i - 1, 2)];
            D[MINDEX2(n, 3, i, 0)] = s;
            D[MINDEX2(n, 3, i, 1)] = s5;
            D[MINDEX2(n, 3, i, 2)] = s6;
          }
        } /*cn@*/
        else {
            /*@generic*/ for (int c = 0; c < cn; c++) {
              uint16_t s = (uint16_t)0;
              for (int i = 0; i < kn; i++) {
                s = s + (uint16_t)S[MINDEX2(n + kn, cn, i, c)];
              }
              D[MINDEX2(n, cn, 0, c)] = s;
              for (int i = 1; i < n; i++) {
                s = s + (uint16_t)S[MINDEX2(n + kn, cn, i + kn - 1, c)] -
                    (uint16_t)S[MINDEX2(n + kn, cn, i - 1, c)];
                D[MINDEX2(n, cn, i, c)] = s;
              }
            } /*generic@*/
        }
      }
    }
  }
}
