#include <optitrust.h>

void f() {
  __pure();

  // FIXME: CALLOC
  int* const a = MALLOC1(int, 10);
  __GHOST_BEGIN(focus, group_focus_subrange, "sub_range := 3..7");
  for (int i = 3; i < 7; i++) {
    __strict();
    __xwrites("&a[MINDEX1(10, i)] ~> Cell");
    a[MINDEX1(10, i)] = 0;
  }
  __GHOST_END(focus);
  free(a);
}
