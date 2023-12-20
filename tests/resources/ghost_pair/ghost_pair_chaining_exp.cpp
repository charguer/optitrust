#include <optitrust.h>

int main() {
  __pure();
  int x = 0;
  float* const B = (float* const)CALLOC2(8, 6, sizeof(float));
  const __ghost_fn focusBi =
      __ghost_begin(group_ro_focus,
                    "i := 1, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  const __ghost_fn focusBj =
      __ghost_begin(group_ro_focus,
                    "i := 2, bound_check_start := checked, bound_check_stop := "
                    "checked, bound_check_step := checked");
  x += B[MINDEX2(8, 6, 1, 2)];
  __ghost_end(focusBj);
  __ghost_end(focusBi);
  MFREE2(8, 6, B);
}
