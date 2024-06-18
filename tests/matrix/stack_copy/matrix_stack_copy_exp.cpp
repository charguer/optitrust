#include <optitrust.h>

void f() {
  __pure();
  float* const s = (float*)MALLOC2(32, 32, sizeof(float));
  for (int i = 0; i < 32; i++) {
    __strict();
    __xwrites("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");
    for (int j = 0; j < 32; j++) {
      __strict();
      __xwrites("&s[MINDEX2(32, 32, i, j)] ~> Cell");
      s[MINDEX2(32, 32, i, j)] = (float)0;
    }
  }
  for (int i = 0; i < 32; i++) {
    __strict();
    __xmodifies("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");
    float* const x = ref[32] float();
    const __ghost_fn __ghost_pair_1 =
        __ghost_begin(mindex2_contiguous_ro, "M := s");
    MMEMCPY(x, 0, s, i * 32 + 0, 32, sizeof(float));
    __ghost_end(__ghost_pair_1);
    for (int j = 0; j < 32; j++) {
      __strict();
      __xmodifies("&x[MINDEX1(32, j)] ~> Cell");
      for (int k = 0; k < 4; k++) {
        __strict();
        __smodifies("&x[MINDEX1(32, j)] ~> Cell");
        x[MINDEX1(32, j)] += (float)k;
      }
    }
    __ghost(mindex2_contiguous_uninit, "M := s");
    MMEMCPY(s, i * 32 + 0, x, 0, 32, sizeof(float));
    __ghost(mindex2_contiguous_rev, "M := s");
  }
  for (int i = 0; i < 32; i++) {
    __strict();
    __xreads("for j in 0..32 -> &s[MINDEX2(32, 32, i, j)] ~> Cell");
    for (int j = 0; j < 32; j++) {
      __strict();
      __xreads("&s[MINDEX2(32, 32, i, j)] ~> Cell");
    }
  }
  MFREE2(32, 32, s);
}
