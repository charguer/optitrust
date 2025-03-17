#include <optitrust.h>

__ghost(assert_inhabited, "x := arbitrary(int -> Prop)", "Triv <- x");

__ghost_ret trivial_init() {
  __requires("k: int");
  __ensures("triv: Triv(k)");
  __admitted();
}

__ghost_ret trivial_change() {
  __requires("k: int");
  __requires("old_k: int");
  __requires("old_triv: Triv(old_k)");
  __ensures("triv: Triv(k)");
  __admitted();
}

void req_triv(int k) { __requires("triv: Triv(k)"); }

void f() {
  __pure();
  const int k = 0;
  /*@ m0 @*/
  k + 1;
  /*@ m1, m2 @*/
  k + 2;
  /*@ m3, m4, fission, m5 @*/
  __ghost(trivial_init, "k := k", "#_1 <- triv");
  __ghost(trivial_change, "k := k + 3", "#_2 <- triv");
  __clear("#_1");
  req_triv(k + 3);
  __clear("#_2");
  /*@ fission, m6 @*/
  __ghost(trivial_init, "k := k", "#_3 <- triv");
  __ghost(trivial_change, "k := k + 3", "#_4 <- triv");
  __ghost(trivial_change, "k := k + 4", "#_5 <- triv");
  req_triv(k + 4);
  /*@ m7 @*/
}

void req_refl(int k) { __requires("refl: __is_true(k == k)"); }

void let_ghost(int k) {
  __pure();
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k <- proof");
  req_refl(k);
  __with("refl := refl_k");
  __clear("refl_k");
  /*@ fission @*/
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k1 <- proof");
  req_refl(k);
  __with("refl := refl_k1");
  __clear("refl_k1");
  /*@ fission @*/
}

void with_clear(int k) {
  __ensures("__is_true(k == k)");
  /*@ fission @*/
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k2 <- proof");
  __ghost(assert_prop, "P := __is_true(k == k)", "refl_k3 <- proof");
}
