#include <optitrust.h>

typedef uint8_t T;

typedef uint16_t ST;

void rowSum_cn1(const int kn, const T* S, ST* D, const int n) {
  ST s = 0;
  for (int i = 0; i < kn; i++) {
    s += (ST)S[MINDEX2(n + kn, 1, i, 0)];
  }
  D[MINDEX2(n, 1, 0, 0)] = s;
  for (int i = 0; i < n - 1; i++) {
    s -= (ST)S[MINDEX2(n + kn, 1, i, 0)];
    s += (ST)S[MINDEX2(n + kn, 1, i + kn, 0)];
    D[MINDEX2(n, 1, i + 1, 0)] = s;
  }
}

void rowSum_cn3(const int kn, const T* S, ST* D, const int n) {
  ST s = 0;
  ST s1 = 0;
  ST s3 = 0;
  for (int i = 0; i < kn; i++) {
    s += (ST)S[MINDEX2(n + kn, 3, i, 0)];
    s1 += (ST)S[MINDEX2(n + kn, 3, i, 1)];
    s3 += (ST)S[MINDEX2(n + kn, 3, i, 2)];
  }
  D[MINDEX2(n, 3, 0, 0)] = s;
  D[MINDEX2(n, 3, 0, 1)] = s1;
  D[MINDEX2(n, 3, 0, 2)] = s3;
  for (int i = 0; i < n - 1; i++) {
    s -= (ST)S[MINDEX2(n + kn, 3, i, 0)];
    s += (ST)S[MINDEX2(n + kn, 3, i + kn, 0)];
    D[MINDEX2(n, 3, i + 1, 0)] = s;
    s1 -= (ST)S[MINDEX2(n + kn, 3, i, 1)];
    s1 += (ST)S[MINDEX2(n + kn, 3, i + kn, 1)];
    D[MINDEX2(n, 3, i + 1, 1)] = s1;
    s3 -= (ST)S[MINDEX2(n + kn, 3, i, 2)];
    s3 += (ST)S[MINDEX2(n + kn, 3, i + kn, 2)];
    D[MINDEX2(n, 3, i + 1, 2)] = s3;
  }
}

void rowSumOpt(const int kn, const T* S, ST* D, const int n, const int cn) {
  if (cn == 1) {
    rowSum_cn1(kn, S, D, n);
  } else if (cn == 3) {
    rowSum_cn3(kn, S, D, n);
  } else {
    for (int c = 0; c < cn; c++) {
      ST s = 0;
      for (int i = 0; i < kn; i++) {
        s += (ST)S[MINDEX2(n + kn, cn, i, c)];
      }
      D[MINDEX2(n, cn, 0, c)] = s;
      for (int i = 0; i < n - 1; i++) {
        s -= (ST)S[MINDEX2(n + kn, cn, i, c)];
        s += (ST)S[MINDEX2(n + kn, cn, i + kn, c)];
        D[MINDEX2(n, cn, i + 1, c)] = s;
      }
    }
  }
}
