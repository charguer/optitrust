#include <optitrust_models.h>

void test_var() {
  __pure();
  double x = 0. + 5.;
  x = 1. + 5.;
  double y = (x - 5.) * 1.;
  x = (x - 5.) * 2. + 5.;
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");
  int s = 0 + 1;
  for (int i = 0; i < n; i++) {
    __strict();
    __spreserves("&s ~~> i + 1");
    s++;
  }
}
