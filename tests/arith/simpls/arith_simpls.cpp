#include <optitrust.h>

void rowSum(const int kn, const int* S, int* D, const int n, const int cn) {
  for (int ic = 0; ic < n * cn; ic++) {
    D[cn * (ic / cn) + ic % cn] =
        S[cn * (ic / cn) + ic % cn] +
        S[cn * (ic / cn + 1) + ic % cn];
  }
}

void box_filter_rowsum() {
  const int n = 0;
  const int m = 0;
  const int p = 0;
  const int r = 0;
  int q;
  q = exact_div((n * m * 4 * 32), m); // = 128 * n
  q = (exact_div(n, m) + p) * m + r; // = n + p * m + r
  q = 4 * (exact_div(n, 4) + p) + 2; // = n + p * 4 + 2
  q = (1 + exact_div(n, m)) * m + r; // = m + n + r
  q = 3 * (exact_div(n, 3) + 1) + 2; // = n + 5
}
