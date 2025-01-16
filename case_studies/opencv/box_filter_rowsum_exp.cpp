#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum(const int w, const uint8_t* S, uint16_t* D, const int n,
            const int cn) {
  if (w == 3) {
    for (int ic = 0; ic < cn * n; ic++) {
      D[ic] = (uint16_t)S[ic] + (uint16_t)S[cn + ic] + (uint16_t)S[2 * cn + ic];
    }
  } else {
    if (w == 5) {
      for (int ic = 0; ic < cn * n; ic++) {
        D[ic] = (uint16_t)S[ic] + (uint16_t)S[cn + ic] +
                (uint16_t)S[2 * cn + ic] + (uint16_t)S[3 * cn + ic] +
                (uint16_t)S[4 * cn + ic];
      }
    } else {
      if (cn == 1) {
        uint16_t s = (uint16_t)0;
        for (int i = 0; i < w; i += 1) {
          s += (uint16_t)S[i];
        }
        D[0] = s;
        for (int i = 0; i < n - 1; i += 1) {
          s += (uint16_t)S[i + w] - (uint16_t)S[i];
          D[i + 1] = s;
        }
      } else {
        if (cn == 3) {
          uint16_t s = (uint16_t)0;
          uint16_t s2 = (uint16_t)0;
          uint16_t s3 = (uint16_t)0;
          for (int i = 0; i < 3 * w; i += 3) {
            s += (uint16_t)S[i];
            s2 += (uint16_t)S[i + 1];
            s3 += (uint16_t)S[i + 2];
          }
          D[0] = s;
          D[1] = s2;
          D[2] = s3;
          for (int i = 0; i < 3 * n - 3; i += 3) {
            s += (uint16_t)S[3 * w + i] - (uint16_t)S[i];
            s2 += (uint16_t)S[3 * w + i + 1] - (uint16_t)S[i + 1];
            s3 += (uint16_t)S[3 * w + i + 2] - (uint16_t)S[i + 2];
            D[i + 3] = s;
            D[i + 4] = s2;
            D[i + 5] = s3;
          }
        } else {
          if (cn == 4) {
            uint16_t s = (uint16_t)0;
            uint16_t s4 = (uint16_t)0;
            uint16_t s5 = (uint16_t)0;
            uint16_t s6 = (uint16_t)0;
            for (int i = 0; i < 4 * w; i += 4) {
              s += (uint16_t)S[i];
              s4 += (uint16_t)S[i + 1];
              s5 += (uint16_t)S[i + 2];
              s6 += (uint16_t)S[i + 3];
            }
            D[0] = s;
            D[1] = s4;
            D[2] = s5;
            D[3] = s6;
            for (int i = 0; i < 4 * n - 4; i += 4) {
              s += (uint16_t)S[4 * w + i] - (uint16_t)S[i];
              s4 += (uint16_t)S[4 * w + i + 1] - (uint16_t)S[i + 1];
              s5 += (uint16_t)S[4 * w + i + 2] - (uint16_t)S[i + 2];
              s6 += (uint16_t)S[4 * w + i + 3] - (uint16_t)S[i + 3];
              D[i + 4] = s;
              D[i + 5] = s4;
              D[i + 6] = s5;
              D[i + 7] = s6;
            }
          } else {
            for (int c = 0; c < cn; c++) {
              uint16_t s = (uint16_t)0;
              for (int i = 0; i < cn * w; i += cn) {
                s += (uint16_t)S[c + i];
              }
              D[c] = s;
              for (int i = c; i < cn * n - cn + c; i += cn) {
                s += (uint16_t)S[w * cn + i] - (uint16_t)S[i];
                D[cn + i] = s;
              }
            }
          }
        }
      }
    }
  }
}
