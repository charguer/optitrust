#include <optitrust.h>

void f1(float* t, int n) {
  __modifies("t ~> Matrix1(n)");
  for (int i = 0; i < n; ++i) {
    __strict();
    __xmodifies("&t[IDX(i)] ~> Cell");
    t[IDX(i)]++;
  }
}

// void f2(float* t, int n, int m) {
//   __modifies("t ~> Matrix2(n, m)");

//   for (int i = 0; i < n; ++i) {
//     __strict();
//     __xmodifies("&t[IDX(i,j)] ~> Cell");
//     t[IDX(i,j)]++;
//   }
// }

