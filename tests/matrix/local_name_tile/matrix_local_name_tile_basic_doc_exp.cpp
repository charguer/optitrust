#include <optitrust.h>

void f() {
  __pure();
  int* const a = (int*)malloc(MSIZE1(10) * sizeof(int));
  const __ghost_fn focus =
      __ghost_begin(group_focus_subrange, "sub_range := 3..7");
  int* const b = (int*)malloc(MSIZE1(7 - 3) * sizeof(int));
  __ghost([&]() {
    __consumes("b ~> UninitMatrix1(7 - 3)");
    __produces("for i1 in 3..7 -> &b[MINDEX1(7 - 3, i1 - 3)] ~> UninitCell");
    __admitted();
    __with("justif := shift_groups");
  });
  for (int i = 3; i < 7; i++) {
    __strict();
    __xwrites("&b[MINDEX1(7 - 3, i - 3)] ~> Cell");
    b[MINDEX1(7 - 3, i - 3)] = 0;
  }
  __ghost([&]() {
    __consumes("for i1 in 3..7 -> &b[MINDEX1(7 - 3, i1 - 3)] ~> UninitCell");
    __produces("b ~> UninitMatrix1(7 - 3)");
    __admitted();
    __with("justif := shift_groups");
  });
  free(b);
  __ghost_end(focus);
  free(a);
}
