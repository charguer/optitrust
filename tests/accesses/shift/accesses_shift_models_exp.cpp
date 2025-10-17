#include <optitrust_models.h>

void test_var() {
  __pure();
  int x = 0 + 5;
  x = 1 + 5;
  int y = /*no-brace*/ { int get = x - 5;
  __ghost(
      rewrite_linear,
      "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(1, 5)");
  const int getc = get;
}
*1;
x = /*no-brace*/ { int get = x - 5;
__ghost(
    rewrite_linear,
    "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(1, 5)");
const int getc = get;
}
*2 + 5;
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");
  int r = 0;
  {
    int s = 0 + 1;
    for (int i = 0; i < n; i++) {
      __strict();
      __spreserves("&s ~~> i + 1");
      s++;
    }
    r = /*no-brace*/ { int get = s - 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &get ~~> v, by := "
            "z_cancel_plus_minus(n, 1)");
    const int getc = get;
  };
  __call_with(assert_hprop(), "H := &r ~~> n");
}
{
  int s = 0 + 1;
  for (int i = 0; i < n; i++) {
    __strict();
    __spreserves("&s ~~> i + 1");
    s++;
    s = s - 1 - 1 + 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &s ~~> v - 1 + 1, by := "
            "z_cancel_plus_minus(i + 1, 1)");
    s = s - 1 + 1 + 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
            "z_cancel_plus_minus(i + 1 - 1, 1)");
    __ghost(
        rewrite_linear,
        "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + 1, 1)");
    s = /*no-brace*/ { int get = s - 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(i "
            "+ 1, 1)");
    const int getc = get;
  }
  -1 + 1;
  s = /*no-brace*/ { int get = s - 1;
  __ghost(rewrite_linear,
          "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(i + "
          "1 - 1, 1)");
  const int getc = get;
}
+ 1 + 1;
__ghost(rewrite_linear,
        "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + 1, 1)");
}
s = /*no-brace*/ { int get = s - 1;
__ghost(
    rewrite_linear,
    "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(n, 1)");
const int getc = get;
}
+ 1 + 1;
s = s - 1 + 1 + 1;
__call_with(rewrite_linear(),
            "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
            "z_cancel_plus_minus(n + 1, 1)");
s++;
r = /*no-brace*/ { int get = s - 1;
__ghost(rewrite_linear,
        "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(n + 1 "
        "+ 1 + 1, 1)");
const int getc = get;
}
;
__call_with(assert_hprop(), "H := &r ~~> n + 1 + 1 + 1");
}
{
  __call_with(assume(), "P := forall (n: int) -> __is_true(n - 1 + 2 == n + 1)",
              "p1 <- H");
  __call_with(assume(), "P := __is_true(1 == 0 + 1)", "p01 <- H");
  int s = 1 + 1;
  __call_with(rewrite_linear(), "inside := fun v -> &s ~~> v + 1, by := p01");
  for (int i = 0; i < n; i++) {
    __strict();
    __spreserves("&s ~~> i + 1 + 1");
    s = /*no-brace*/ { int get = s - 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &get ~~> v, by := z_cancel_plus_minus(i "
            "+ 1, 1)");
    const int getc = get;
  }
  -1 + 2 + 1;
  __ghost(rewrite_linear, "inside := fun v -> &s ~~> v + 1, by := p1(i + 1)");
}
  }
}
