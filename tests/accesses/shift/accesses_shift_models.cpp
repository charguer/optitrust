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

  int s2 = 1;
  __ghost(assume, "1 = 0 + 1", "p01 <- H");
  __ghost(assume, "forall (n : int) -> n - 1 + 2 = n + 1", "p1 <- H");
  __ghost(rewrite_linear, "inside := fun v -> &s2 ~~> v, by := p01");
  for (int i = 0; i < n; i++) {
    __spreserves("&s2 ~~> i + 1");
    s2 = s2 - 1 + 2;
    __ghost(rewrite_linear, "inside := fun v -> &s2 ~~> v, by := p1(i + 1)");
  }
}
