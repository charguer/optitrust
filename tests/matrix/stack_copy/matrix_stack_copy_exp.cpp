#include <optitrust.h>

void f() {
__pure();
  float* const s = (float* const)MALLOC2(32, 32, sizeof(float));
  for (int i = 0; i < 32; i++) {
    __consumes(
        "_Uninit(Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> "
        "Cell))");
    __produces(
        "Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");
    for (int j = 0; j < 32; j++) {
      __consumes("_Uninit(&s[MINDEX2(32, 32, i, j)] ~> Cell)");
      __produces("&s[MINDEX2(32, 32, i, j)] ~> Cell");
      s[MINDEX2(32, 32, i, j)] = 0;
    }
  }
  for (int i = 0; i < 32; i++) {
    __modifies(
        "Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");
    float* const x = new float(32)();
    const __ghost_fn __ghost_pair_1 =
        __ghost_begin(mindex2_contiguous_ro, "M := s");
    MMEMCPY(x, 0, s, i * 32 + 0, 32, sizeof(float));
__ghost_end(__ghost_pair_1);
    for (int j = 0; j < 32; j++) {
__modifies("&x[MINDEX1(32, j)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __sequentially_modifies("&x[MINDEX1(32, j)] ~> Cell");
        x[MINDEX1(32, j)] += k;
      }
    }
    __ghost(mindex2_contiguous_uninit, "M := s");
    MMEMCPY(s, i * 32 + 0, x, 0, 32, sizeof(float));
__ghost(mindex2_contiguous_rev, "M := s");
  }
  for (int i = 0; i < 32; i++) {
__reads(
        "Group(range(0, 32, 1), fun j -> &s[MINDEX2(32, 32, i, j)] ~> Cell)");
    for (int j = 0; j < 32; j++) {
      __reads("&s[MINDEX2(32, 32, i, j)] ~> Cell");
    }
      }
  MFREE2(32, 32, s);
  }
