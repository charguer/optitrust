#include <optitrust.h>

__ghost_ret trivial_init() {
  __requires("k: int");
  __ensures("__is_true(k == k)");
  __admitted();
}

__ghost_ret trivial_change() {
  __requires("k: int");
  __requires("old_k: int");
  __requires("__is_true(old_k == old_k)");
  __ensures("__is_true(k == k)");
  __admitted();
}

void req_triv(int k) { __requires("__is_true(k == k)"); }

void f() {
  __pure();
  const int k = 0;
  /*@ m0 @*/
  k + 1;
  /*@ m1, m2 @*/
  k + 2;
  /*@ m3 @*/
  __ghost(trivial_init, "k := k");
  __ghost(trivial_change, "k := k + 3");
  /*@ m4, m5 @*/
  req_triv(k + 3);
  /*@ m6 @*/
  __ghost(trivial_change, "k := k + 4");
  req_triv(k + 4);
  /*@ m7 @*/
}

void g() {
  __pure();
  for (int i = 0; i < 100; ++i) {
    __strict();
    __xensures("__is_true(i == i)");
    __ghost(trivial_init, "k := i + 12");
    req_triv(i + 12);
    __ghost(trivial_init, "k := i");
  }
}

void must_be_zero(int i) { __requires("__is_true(i == 0)"); }

void must_be_zero_ens(int i) {
  __requires("__is_true(i == 0)");
  __ensures("__is_true(i * 1 == 0)");
  __admitted();
}

void h(int i, int j) {
  __requires("__is_true(i == j)");
  if (j == 0) {
    __ghost(assert_alias, "x := j, y := 0");
    __ghost(assert_alias, "x := j, y := 0");
    must_be_zero(i);
    __ghost(assert_alias, "x := j, y := 0");
  }
}

void h2(int i, int j) {
  __requires("__is_true(i == j)");
  if (j == 0) {
    __ghost(assert_alias, "x := j, y := 0");
    __ghost(assert_alias, "x := j, y := 0");
    must_be_zero_ens(i);
    __ghost(assert_alias, "x := j, y := 0");
    must_be_zero(i * 1);
  }
}
