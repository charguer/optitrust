#include <optitrust.h>

__ghost_ret freeze_cell() {
  __requires("T: Type");
  __requires("p: ptr(T)");
  __consumes("p ~> Cell");
  __produces("for #_1 in 0..1 -> p ~> Cell");
  __admitted();
}

__ghost_ret unfreeze_cell() {
  __reverts(freeze_cell);
  __admitted();
}

void f(float* M, int n) {
  __modifies("for i in 0..n -> &M[i] ~> Cell");
  __ghost([&]() {
    __consumes("for i in 0..n -> &M[i] ~> Cell");
    __produces("for i in 0..n -> for #_1 in 0..1 -> &M[i] ~> Cell");
    for (int i = 0; i < n; ++i) {
      __strict();
      __xconsumes("&M[i] ~> Cell");
      __xproduces("for #_1 in 0..1 -> &M[i] ~> Cell");
      __ghost(freeze_cell, "p := &M[i]");
    }
  });
  __ghost([&]() {
    __consumes("for i in 0..n -> for #_1 in 0..1 -> &M[i] ~> Cell");
    __produces("for i in 0..n -> &M[i] ~> Cell");
    for (int i = 0; i < n; ++i) {
      __strict();
      __xconsumes("for #_1 in 0..1 -> &M[i] ~> Cell");
      __xproduces("&M[i] ~> Cell");
      __ghost(unfreeze_cell, "p := &M[i]");
    }
  });
}
