#include <optitrust.h>

void rowSum(int kn, int* S, int* D, int n, int cn) {
  for (int ic = 0; ic < n * cn; ic += 1) {
    D[ic] = S[ic] + S[ic + cn];
  }
}

void box_filter_rowsum() {
  const int n = 0;
  const int m = 0;
  const int p = 0;
  const int r = 0;
  int q;
  q = 128 * n;
  q = n + p * m + r;
  q = 2 + n + 4 * p;
  q = m + n + r;
  q = 5 + n;
}
