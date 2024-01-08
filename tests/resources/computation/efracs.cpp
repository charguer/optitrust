#include <optitrust.h>

__GHOST(ro_fork_group_copy) {
  __requires("f: _Fraction, H: formula, r: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / range_count(r), Group(r, fun _ -> H))");

  __ghost(ro_fork_group, "");
  // Here we have: forall g <= f, _RO(g / range_count(r), Group(r, fun _ -> H)), _RO(f - g, H)
  // We need to find g = f
}

// _RO(?g/r, _) * _RO(f - ?g, _) ==> _RO(f / range(r), _)

__GHOST(double_ro_fork_group) {
  __requires("f: _Fraction, H: formula, r1: range, r2: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 2 / range_count(r1), Group(r1, fun _ -> H)), _RO(f / 2 / range_count(r2), Group(r2, fun _ -> H))");

  __ghost(ro_fork_group, "r := r1, H := H");
  __ghost(ro_fork_group, "r := r2, H := H");
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(h / range_count(r2), _), _RO(f - g - h, H)
  // We need to find g = f/2, h = f/2
}

// More complex case: I guess it should fail but currently it passes...
void weird(int* k, int n1, int n2) {
  __requires("f: _Fraction");
  __consumes("_RO(f, k ~> Cell)");
  __produces("_RO(f / range_count(range(0, n1, 1)), Group(range(0, n1, 1), fun _ -> k ~> Cell))");

  __ghost(ro_fork_group, "r := range(0, n1, 1)");
  // forall g <= f, _RO(g / range_count(r1), Group(r, fun _ -> H)), _RO(f - g, H)
  __GHOST_BEGIN(second_fork, ro_fork_group, "r := range(0, n2, 1), H := k ~> Cell");
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(h / range_count(r2), _), _RO(f - g - h, H)
  int acc = 0;
  for (int i = 0; i < n1; ++i) {
    __reads("k ~> Cell");
    __sequentially_modifies("&acc ~> Cell");
    acc += *k;
  }
  for (int j = 0; j < n2; ++j) {
    __reads("k ~> Cell");
    __sequentially_modifies("&acc ~> Cell");
    acc += *k;
  }
  __GHOST_END(second_fork);
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(f - g, H)
  // forall (g < f), _RO(g / range_count(r1), _), _RO(f - g, H) // autoclean
  // _RO(f / range_count(r1), _), _RO(f - g, H)
}
