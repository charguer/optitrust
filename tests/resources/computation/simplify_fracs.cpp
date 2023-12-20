#include <optitrust.h>

__GHOST(noop) {
  __pure();
}

__GHOST(simplify_fracs) {
  __requires("f: _Fraction, g: _Fraction, h: _Fraction, H: formula");
  __consumes("H1: _RO(f - g, H), H2: _RO(h - f, H), H3: _RO(g, H)");
  __produces("H: _RO(h, H)");

  __ghost(noop, "");
}

__GHOST(two_base_res) {
  __requires("f: _Fraction, g: _Fraction, H1: formula, H2: formula");
  __consumes("H1f: _RO(f - g, H1), H2f: _RO(f, H2), H1g: _RO(g, H1), H2g: _RO(g - f, H2)");
  __produces("H1f: _RO(f, H1), H2g: _RO(g, H2)");

  __ghost(noop, "");
}
