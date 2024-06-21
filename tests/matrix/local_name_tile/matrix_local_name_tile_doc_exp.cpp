#include <optitrust.h>

void f() {
  __pure();
  int* const a = (int*)MALLOC1(10, sizeof(int));
  const __ghost_fn focus =
      __ghost_begin(group_focus_subrange_uninit, "sub_range := 3..7");
  int* const b = (int*)MALLOC1(4, sizeof(int));
  __ghost(
      [&]() {
        __consumes("_Uninit(b ~> Matrix1(4))");
        __produces("_Uninit(for i1 in 3..7 -> &b[MINDEX1(4, i1 - 3)] ~> Cell)");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  for (int i = 3; i < 7; i++) {
    __strict();
    __xwrites("&b[MINDEX1(4, i - 3)] ~> Cell");
    b[MINDEX1(4, i - 3)] = 0;
  }
  __ghost(
      [&]() {
        __consumes("_Uninit(for i1 in 3..7 -> &b[MINDEX1(4, i1 - 3)] ~> Cell)");
        __produces("_Uninit(b ~> Matrix1(4))");
        __admitted();
        __with("justif := shift_groups");
      },
      "");
  MFREE1(4, b);
  __ghost_end(focus);
  MFREE1(10, a);
}
