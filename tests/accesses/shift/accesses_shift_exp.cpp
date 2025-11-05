#include <optitrust.h>

void test_var() {
  __pure();
  double x = 0. + 5.;
  x = 1. + 5.;
  double y = (x - 5.) * 1.;
  x = (x - 5.) * 2. + 5.;
}

void test_var_inv(int* t, int n) {
  __reads("t ~> Matrix1(n)");
  int s = 0 + 1;
  for (int i = 0; i < n; i++) {
    __strict();
    __smodifies("&s ~> Cell");
    s = s - 1 + 1 + 1;
  }
}

void test_array() {
  double t[2] = {1., 2};
  t[0] = t[0] * 1.;
}

int main() {
  double t[3] = {1., 2., 3.};
  double v = 2.;
  int i = 0;
  t[i] = t[i] * v;
  return 0;
}
