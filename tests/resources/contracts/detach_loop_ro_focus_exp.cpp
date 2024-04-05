#include <optitrust.h>

void f(int* M) {
  __reads("M ~> Matrix1(10)");
  int acc = 0;
  for (int i = 0; i < 10; ++i) {
    __strict();
    __smodifies("&acc ~> Cell");
    __sreads("M ~> Matrix1(10)");
    const __ghost_fn __ghost_pair_1 = __ghost_begin(
        group_ro_focus, "i := i, items := fun i -> &M[MINDEX1(10, i)] ~> Cell");
    acc += M[MINDEX1(10, i)];
    __ghost_end(__ghost_pair_1);
  }
}
