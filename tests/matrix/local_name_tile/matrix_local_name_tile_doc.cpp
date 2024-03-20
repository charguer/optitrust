#include <optitrust.h>

void f() {
  __pure();

  // TODO: deal with CALLOC
  int* const a = (int* const) MALLOC1(10, sizeof(int));
  __GHOST_BEGIN(focus, group_focus_subrange_uninit, "sub_range := 3..7");
  for (int i = 3; i < 7; i++) {
    __strict();
    __writes("&a[MINDEX1(10, i)] ~> Cell");
    a[MINDEX1(10, i)] = 0;
  }
  __GHOST_END(focus);
  MFREE1(10, a);
}
