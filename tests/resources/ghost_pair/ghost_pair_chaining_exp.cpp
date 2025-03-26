#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  int* const B = (int*)calloc(MSIZE2(8, 6), sizeof(int));
  const __ghost_fn focusBi = __ghost_begin(ro_group_focus, "i := 1");
  const __ghost_fn focusBj = __ghost_begin(ro_group_focus, "i := 2");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(focusBj);
  __ghost_end(focusBi);
  free(B);
}
