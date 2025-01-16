#include <optitrust.h>

void f() {
  __pure();
  // test integer division
  int n = 1; int m = 1; int p = 1; int q = 1;
  int x = 1; int y = 1; int i = 1; int j = 1;
  x = 3 + n; // x = n + 3
  x = 2 + 2 * n + 4 + m; // x = 2 * n + m + 6
  y = j * i + q + 3 + p + 2 * i + 2 * j; // x = i*j + 2*i + 2*j + p + q + 3;
  y = 2 + (m * 4 * n); // y = 4 * m * n + 2
}


