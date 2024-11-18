#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int kn, const uint8_t* S, uint16_t* D, const int n,
            const int cn) {
  if (kn == 3) /*@kn*/ {
    for (int ic = 0; ic < n * cn; ic++) {
      D[ic] = (uint16_t)S[ic] + (uint16_t)S[ic % cn + (1 + ic / cn) * cn] +
              (uint16_t)S[ic % cn + (2 + ic / cn) * cn];
    }
  } /*kn@*/
  else {
    if (kn == 5) /*@kn*/ {
      for (int ic = 0; ic < n * cn; ic++) {
        D[ic] = (uint16_t)S[ic] + (uint16_t)S[ic % cn + (1 + ic / cn) * cn] +
                (uint16_t)S[ic % cn + (2 + ic / cn) * cn] +
                (uint16_t)S[ic % cn + (3 + ic / cn) * cn] +
                (uint16_t)S[ic % cn + (4 + ic / cn) * cn];
      }
    } /*kn@*/
    else /*@nokn*/ {
      if (cn == 1) /*@cn*/ {
        uint16_t s = (uint16_t)0;
        for (int i = 0; i < kn; i += 1) {
          s += (uint16_t)S[i];
        }
        D[0] = s;
        for (int i = 0; i < -1 + n; i += 1) {
          s = s + (uint16_t)S[i + kn] - (uint16_t)S[i];
          D[1 + i] = s;
        }
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          uint16_t s = (uint16_t)0;
          uint16_t s2 = (uint16_t)0;
          uint16_t s3 = (uint16_t)0;
          for (int i = 0; i < 3 * kn; i += 3) {
            s += (uint16_t)S[i];
            s2 += (uint16_t)S[1 + i];
            s3 += (uint16_t)S[2 + i];
          }
          D[0] = s;
          D[1] = s2;
          D[2] = s3;
          for (int i = 0; i < 3 * -1 + 3 * n; i += 3) {
            s = s + (uint16_t)S[i + 3 * kn] - (uint16_t)S[i];
            s2 = s2 + (uint16_t)S[3 * (exact_div(i, 3) + kn) + 1] -
                 (uint16_t)S[1 + i];
            s3 = s3 + (uint16_t)S[3 * (exact_div(i, 3) + kn) + 2] -
                 (uint16_t)S[2 + i];
            D[i + 3] = s;
            D[3 * (exact_div(i, 3) + 1) + 1] = s2;
            D[3 * (exact_div(i, 3) + 1) + 2] = s3;
          }
        } /*cn@*/
        else {
          if (cn == 4) /*@cn*/ {
            uint16_t s = (uint16_t)0;
            uint16_t s4 = (uint16_t)0;
            uint16_t s5 = (uint16_t)0;
            uint16_t s6 = (uint16_t)0;
            for (int i = 0; i < 4 * kn; i += 4) {
              s += (uint16_t)S[i];
              s4 += (uint16_t)S[1 + i];
              s5 += (uint16_t)S[2 + i];
              s6 += (uint16_t)S[3 + i];
            }
            D[0] = s;
            D[1] = s4;
            D[2] = s5;
            D[3] = s6;
            for (int i = 0; i < 4 * -1 + 4 * n; i += 4) {
              s = s + (uint16_t)S[i + 4 * kn] - (uint16_t)S[i];
              s4 = s4 + (uint16_t)S[4 * (exact_div(i, 4) + kn) + 1] -
                   (uint16_t)S[1 + i];
              s5 = s5 + (uint16_t)S[4 * (exact_div(i, 4) + kn) + 2] -
                   (uint16_t)S[2 + i];
              s6 = s6 + (uint16_t)S[4 * (exact_div(i, 4) + kn) + 3] -
                   (uint16_t)S[3 + i];
              D[i + 4] = s;
              D[4 * (exact_div(i, 4) + 1) + 1] = s4;
              D[4 * (exact_div(i, 4) + 1) + 2] = s5;
              D[4 * (exact_div(i, 4) + 1) + 3] = s6;
            }
          } /*cn@*/
          else {
            for (int c = 0; c < cn; c++) {
              uint16_t s = (uint16_t)0;
              for (int i = 0; i < cn * kn; i += cn) {
                s += (uint16_t)S[i + c];
              }
              D[c] = s;
              for (int i = 0; i < -cn + cn * n; i += cn) {
                s = s + (uint16_t)S[(exact_div(i, cn) + kn) * cn + c] -
                    (uint16_t)S[i + c];
                D[(1 + exact_div(i, cn)) * cn + c] = s;
              }
          }
          }
        }
      }
    } /*nokn@*/
  }
}
