#include <optitrust_models.h>

void test_var() {
  __pure();

  // FIXME: support double, etc
  int x = 0;
  x = 1;
  int y = x * 1;
  x = x * 2;
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");

  int r = 0;
  {
    int s = 0;
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i");
      s++;
    }
    r = s;
    __ghost(assert_hprop, "&r ~~> n");
  }
  {
    int s = 0;
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i");
      s++;
      s -= 1;
      s += 1;
      __ghost(rewrite_linear, "inside := fun v -> &s ~~> v, by := z_cancel_minus_plus(i + 1, 1)");
      s = s - 1;
      s = s + 1;
      // __call_with(wrap_z_cancel_minus_plus(s + 1), "n := i + 1, d := 1", "v");
      __ghost(rewrite_linear, "inside := fun v -> &s ~~> v, by := z_cancel_minus_plus(i + 1, 1)");
    }
    s = s + 1;
    s += 1;
    s++;
    r = s;
    __ghost(assert_hprop, "&r ~~> n + 1 + 1 + 1");
  }

  {
    int s = 1;
    __ghost(assume, "1 = 0 + 1", "p01 <- H");
    __ghost(assume, "forall (n : int) -> n - 1 + 2 = n + 1", "p1 <- H");
    __ghost(rewrite_linear, "inside := fun v -> &s ~~> v, by := p01");
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i + 1");
      s = s - 1 + 2;
      __ghost(rewrite_linear, "inside := fun v -> &s ~~> v, by := p1(i + 1)");
    }
  }
}
