#include <optitrust.h>

__GHOST(freeze_cell) {
  __requires("T: Type, p: ptr(T)");
  __consumes("p ~> Cell");
  __produces("for _ in 0..1 -> p ~> Cell");
  __admitted();
}

__GHOST(unfreeze_cell) {
  __reverts(freeze_cell);
  __admitted();
}

void f() {
  __pure();

  [&]() -> void { __pure(); }();

  int x = 0;
  int y = 0;
  [&](int* a) -> void {
    __modifies("a ~> Cell, &x ~> Cell");
    x += 1;
    (*a) += 1;
  }(&y);

  __GHOST_BEGIN(pair, __with_reverse([&]{
    __consumes("&x ~> Cell");
    __produces("for _ in 0..1 -> &x ~> Cell");
    __ghost(freeze_cell, "p := &x"); },
    [&]{ __ghost(unfreeze_cell, "p := &x"); }));
  __GHOST_END(pair);
}
