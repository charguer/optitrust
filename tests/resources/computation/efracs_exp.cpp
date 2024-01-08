#include <optitrust.h>

__ghost_ret ro_fork_group_copy() {
  __requires("f: _Fraction");
  __requires("H: formula");
  __requires("r: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / range_count(r), Group(r, fun _ -> H))");
  __ghost(ro_fork_group, "");
}

__ghost_ret double_ro_fork_group() {
  __requires("f: _Fraction");
  __requires("H: formula");
  __requires("r1: range");
  __requires("r2: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 2 / range_count(r1), Group(r1, fun _ -> H))");
  __produces("_RO(f / 2 / range_count(r2), Group(r2, fun _ -> H))");
  __ghost(ro_fork_group, "r := r1, H := H");
  __ghost(ro_fork_group, "r := r2, H := H");
}

void weird(int* k, int n1, int n2) {
  __requires("f: _Fraction");
  __consumes("_RO(f, k ~> Cell)");
  __produces(
      "_RO(f / range_count(range(0, n1, 1)), Group(range(0, n1, 1), fun _ -> k "
      "~> Cell))");
  __ghost(ro_fork_group, "r := range(0, n1, 1)");
  const __ghost_fn second_fork =
      __ghost_begin(ro_fork_group, "r := range(0, n2, 1), H := k ~> Cell");
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
