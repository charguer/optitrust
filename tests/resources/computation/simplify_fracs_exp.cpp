#include <optitrust.h>

__ghost_ret noop() { __pure(); }

__ghost_ret simplify_fracs() {
  __requires("f: _Fraction");
  __requires("g: _Fraction");
  __requires("h: _Fraction");
  __requires("H: formula");
  __consumes("H1: _RO(f - g, H)");
  __consumes("H2: _RO(h - f, H)");
  __consumes("H3: _RO(g, H)");
  __produces("H: _RO(h, H)");
  __ghost(noop, "");
}

__ghost_ret two_base_res() {
  __requires("f: _Fraction");
  __requires("g: _Fraction");
  __requires("H1: formula");
  __requires("H2: formula");
  __consumes("H1f: _RO(f - g, H1)");
  __consumes("H2f: _RO(f, H2)");
  __consumes("H1g: _RO(g, H1)");
  __consumes("H2g: _RO(g - f, H2)");
  __produces("H1f: _RO(f, H1)");
  __produces("H2g: _RO(g, H2)");
  __ghost(noop, "");
}
