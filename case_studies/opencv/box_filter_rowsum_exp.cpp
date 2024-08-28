#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  if (kn == 3) /*@kn*/ {
    for (int ic = 0; ic < n * cn; ic++) {
      D[ic / cn * cn + ic % cn] = (uint16_t)S[ic / cn * cn + ic % cn] +
                                  (uint16_t)S[(1 + ic / cn) * cn + ic % cn] +
                                  (uint16_t)S[(2 + ic / cn) * cn + ic % cn];
    }
  } /*kn@*/
  else {
    if (kn == 5) /*@kn*/ {
      for (int ic = 0; ic < n * cn; ic++) {
        D[ic / cn * cn + ic % cn] = (uint16_t)S[ic / cn * cn + ic % cn] +
                                    (uint16_t)S[(1 + ic / cn) * cn + ic % cn] +
                                    (uint16_t)S[(2 + ic / cn) * cn + ic % cn] +
                                    (uint16_t)S[(3 + ic / cn) * cn + ic % cn] +
                                    (uint16_t)S[(4 + ic / cn) * cn + ic % cn];
      }
    } /*kn@*/
    else {
      if (cn == 1) /*@cn*/ {
        uint16_t s = (uint16_t)0;
        for (int i = 0; i < kn; i++) {
          s = s + (uint16_t)S[i];
        }
        D[0] = s;
        for (int i = 1; i < n; i++) {
          s = s + (uint16_t)S[-1 + i + kn] - (uint16_t)S[-1 + i];
          D[i] = s;
        }
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          uint16_t s = (uint16_t)0;
          uint16_t s5 = (uint16_t)0;
          uint16_t s6 = (uint16_t)0;
          for (int i = 0; i < kn; i++) {
            s = s + (uint16_t)S[3 * i];
            s5 = s5 + (uint16_t)S[1 + 3 * i];
            s6 = s6 + (uint16_t)S[2 + 3 * i];
          }
          D[0] = s;
          D[1] = s5;
          D[2] = s6;
          for (int i = 1; i < n; i++) {
            s = s + (uint16_t)S[3 * (-1 + i + kn)] - (uint16_t)S[3 * (-1 + i)];
            s5 = s5 + (uint16_t)S[1 + 3 * (-1 + i + kn)] -
                 (uint16_t)S[1 + 3 * (-1 + i)];
            s6 = s6 + (uint16_t)S[2 + 3 * (-1 + i + kn)] -
                 (uint16_t)S[2 + 3 * (-1 + i)];
            D[3 * i] = s;
            D[1 + 3 * i] = s5;
            D[2 + 3 * i] = s6;
          }
        } /*cn@*/
        else {
          if (cn == 4) /*@cn*/ {
            uint16_t s = (uint16_t)0;
            uint16_t s7 = (uint16_t)0;
            uint16_t s8 = (uint16_t)0;
            uint16_t s9 = (uint16_t)0;
            for (int i = 0; i < kn; i++) {
              s = s + (uint16_t)S[4 * i];
              s7 = s7 + (uint16_t)S[1 + 4 * i];
              s8 = s8 + (uint16_t)S[2 + 4 * i];
              s9 = s9 + (uint16_t)S[3 + 4 * i];
            }
            D[0] = s;
            D[1] = s7;
            D[2] = s8;
            D[3] = s9;
            for (int i = 1; i < n; i++) {
              s = s + (uint16_t)S[4 * (-1 + i + kn)] -
                  (uint16_t)S[4 * (-1 + i)];
              s7 = s7 + (uint16_t)S[1 + 4 * (-1 + i + kn)] -
                   (uint16_t)S[1 + 4 * (-1 + i)];
              s8 = s8 + (uint16_t)S[2 + 4 * (-1 + i + kn)] -
                   (uint16_t)S[2 + 4 * (-1 + i)];
              s9 = s9 + (uint16_t)S[3 + 4 * (-1 + i + kn)] -
                   (uint16_t)S[3 + 4 * (-1 + i)];
              D[4 * i] = s;
              D[1 + 4 * i] = s7;
              D[2 + 4 * i] = s8;
              D[3 + 4 * i] = s9;
            }
          } /*cn@*/
          else /*@generic*/ {
            for (int c = 0; c < cn; c++) {
              uint16_t s = (uint16_t)0;
              for (int i = 0; i < kn; i++) {
                s = s + (uint16_t)S[i * cn + c];
              }
              D[c] = s;
              for (int i = 1; i < n; i++) {
                s = s + (uint16_t)S[(-1 + i + kn) * cn + c] -
                    (uint16_t)S[(-1 + i) * cn + c];
                D[i * cn + c] = s;
              }
          }
          } /*generic@*/
        }
      }
    }
  }
}
