#include <optitrust.h>

void f() {
__pure();
  int* const a = (int* const)MALLOC1(10, sizeof(int));
  int* const b = (int* const)MALLOC1(7 - 3, sizeof(int));
  __ghost(
      rewrite,
      "H1 := _Uninit(b ~> Matrix1(7 - 3)), H2 := _Uninit(Group(range(3, 7, 1), "
      "fun i1 -> &b[MINDEX1(7 - 3, i1 - 3)] ~> Cell)), by := shift_groups");
  for (int i = 3; i < 7; i++) {
    __consumes("_Uninit(&b[MINDEX1(4, i - 3)] ~> Cell)");
    __produces("&b[MINDEX1(4, i - 3)] ~> Cell");
    b[MINDEX1(4, i - 3)] = 0;
  }
  __ghost(
      rewrite,
      "H1 := _Uninit(Group(range(3, 7, 1), fun i1 -> &b[MINDEX1(7 - 3, i1 - "
      "3)] ~> Cell)), H2 := _Uninit(b ~> Matrix1(7 - 3)), by := shift_groups");
  MFREE1(7 - 3, b);
  MFREE1(10, a);
}
