#include <optitrust_models.h>

void test_var() {
  __pure();
  double x = 0. + 5.;
  __ghost(to_prove_hprop, "H1 := &x ~~> __hole1, H2 := &x ~~> 0. + 5.");
  x = 1. + 5.;
  __ghost(to_prove_hprop, "H1 := &x ~~> __hole2, H2 := &x ~~> 1. + 5.");
  double y = (x - 5.) * 1.;
  x = (x - 5.) * 2. + 5.;
  __ghost(to_prove_hprop, "H1 := &x ~~> __hole3, H2 := &x ~~> 1. * 2. + 5.");
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");
  __ghost(assume, "P := __is_true(1 == 0 + 1)", "p01 <- H");
  __ghost(assume, "P := forall (n: int) -> __is_true(n - 1 + 2 == n + 1)",
          "p1 <- H");
  int s = 0 + 1;
  __ghost(to_prove_hprop, "H1 := &s ~~> __hole4, H2 := &s ~~> 0 + 1");
  for (int i = 0; i < n; i++) {
    __strict();
    __spreserves("&s ~~> i + 1");
    s++;
  }
  int s2 = 1 + 1;
  __ghost(to_prove_hprop, "H1 := &s2 ~~> __hole5, H2 := &s2 ~~> 1 + 1");
  __ghost(rewrite_linear, "inside := fun v -> &s2 ~~> v + 1, by := p01");
  for (int i = 0; i < n; i++) {
    __strict();
    __spreserves("&s2 ~~> i + 1 + 1");
    s2 = s2 - 1 - 1 + 2 + 1;
    __ghost(to_prove_hprop,
            "H1 := &s2 ~~> __hole6, H2 := &s2 ~~> i + 1 - 1 + 2 + 1");
    __ghost(rewrite_linear,
            "inside := fun v -> &s2 ~~> v + 1, by := p1(i + 1)");
  }
}
