#include <optitrust.h>

__GHOST(ro_fork_group_copy) {
  __requires("f: _Fraction, H: formula, r: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / range_count(r), for _ in r -> H)");

  __ghost(ro_fork_group, "");
  // Here we have: forall g <= f, _RO(g / range_count(r), for _ in r -> H), _RO(f - g, H)
  // We need to find g = f
}

// _RO(?g/r, _) * _RO(f - ?g, _) ==> _RO(f / range(r), _)

__GHOST(double_ro_fork_group) {
  __requires("f: _Fraction, H: formula, r1: range, r2: range");
  __consumes("_RO(f, H)");
  __produces("_RO(f / 2 / range_count(r1), for _ in r1 -> H), _RO(f / 2 / range_count(r2), for _ in r2 -> H)");

  __ghost(ro_fork_group, "r := r1, H := H");
  __ghost(ro_fork_group, "r := r2, H := H");
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(h / range_count(r2), _), _RO(f - g - h, H)
  // We need to find g = f/2, h = f/2
}

// More complex case: I guess it should fail but currently it passes...
void weird(int* k, int n1, int n2) {
  __requires("f: _Fraction");
  __consumes("_RO(f, k ~> Cell)");
  __produces("_RO(f / range_count(0..n1), for _ in 0..n1 -> k ~> Cell)");

  __ghost(ro_fork_group, "r := 0..n1");
  // forall g <= f, _RO(g / range_count(r1), for _ in r -> H), _RO(f - g, H)
  __GHOST_BEGIN(second_fork, ro_fork_group, "r := 0..n2, H := k ~> Cell");
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(h / range_count(r2), _), _RO(f - g - h, H)
  int acc = 0;
  for (int i = 0; i < n1; ++i) {
    __strict();
    __xreads("k ~> Cell");
    __smodifies("&acc ~> Cell");
    acc += *k;
  }
  for (int j = 0; j < n2; ++j) {
    __strict();
    __xreads("k ~> Cell");
    __smodifies("&acc ~> Cell");
    acc += *k;
  }
  __GHOST_END(second_fork);
  // forall (g < f) (h <= f - g), _RO(g / range_count(r1), _), _RO(f - g, H)
  // forall (g < f), _RO(g / range_count(r1), _), _RO(f - g, H) // autoclean
  // _RO(f / range_count(r1), _), _RO(f - g, H)
}
