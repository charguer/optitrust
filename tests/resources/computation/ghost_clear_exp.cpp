#include <optitrust.h>

__ghost(assert_inhabited, "x := arbitrary(int -> Prop)", "Triv <- x");

__ghost_ret trivial_init() {
  __requires("k: int");
  __ensures("Triv(k)");
  __admitted();
}

__ghost_ret trivial_change() {
  __requires("k: int");
  __requires("old_k: int");
  __requires("Triv(old_k)");
  __ensures("Triv(k)");
  __admitted();
}

void req_triv(int k) { __requires("Triv(k)"); }

void test_clear(int k) {
  __pure();
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k1 <- proof");
  __clear("refl_k1");
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k2 <- proof");
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k3 <- proof");
}

void clear_in_for() {
  __pure();
  __ghost(trivial_init, "k := 0", "triv_0 <- #170");
  for (int i = 0; i < 20; i++) {
    __strict();
    req_triv(0);
    __clear("triv_0");
  }
  req_triv(0);
}

void clear_in_lambda() {
  __pure();
  __ghost(trivial_init, "k := 0", "triv_0 <- #170");
  auto f() {
    __pure();
    req_triv(0);
    __clear("triv_0");
  }
  req_triv(0);
}

void bind_clear(int k) {
  __pure();
  __ghost(assert_prop, "P := __is_true(k == k)", "_ <- proof");
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k <- proof");
  __ghost(assert_prop, "P := __is_true(k == k)", "_ <- proof");
}
