#include <optitrust.h>

__ghost_ret ro_fork_group_copy() {
  __requires("f: _Fraction");
  __requires("H: formula");
  __requires("r: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / range_count(r), for _ in r -> H)");
  __ghost(ro_fork_group, "");
}

__ghost_ret double_ro_fork_group() {
  __requires("f: _Fraction");
  __requires("H: formula");
  __requires("r1: range");
  __requires("r2: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 2 / range_count(r1), for _ in r1 -> H)");
  __produces("_RO(f / 2 / range_count(r2), for _ in r2 -> H)");
  __ghost(ro_fork_group, "r := r1, H := H");
  __ghost(ro_fork_group, "r := r2, H := H");
}

void weird(int* k, int n1, int n2) {
  __requires("f: _Fraction");
  __consumes("_RO(f, k ~> Cell)");
  __produces("_RO(f / range_count(0..n1), for _ in 0..n1 -> k ~> Cell)");
  __ghost(ro_fork_group, "r := 0..n1");
  const __ghost_fn second_fork =
      __ghost_begin(ro_fork_group, "r := 0..n2, H := k ~> Cell");
  int acc = 0;
  for (int i = 0; i < n1; ++i) {
    __sequentially_modifies("&acc ~> Cell");
    __reads("k ~> Cell");
    acc += *k;
  }
  for (int j = 0; j < n2; ++j) {
    __sequentially_modifies("&acc ~> Cell");
    __reads("k ~> Cell");
    acc += *k;
  }
  __ghost_end(second_fork);
}
