#include <optitrust_models.h>

void test_var() {
  __pure();

  double x = 0.0;
  x = 1.0;
  double y = x * 1.0;
  x = x * 2.0;
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");

  int s = 0;
  for (int i = 0; i < n; i++) {
    __spreserves("&s ~~> i");
    s++;
  }
}
