#include <optitrust.h>

void outer_alloc(int n, int m, int* p) {
  __modifies("for i1 in 0..n -> for i2 in 0..m -> &p[i1 * m + i2] ~> Cell");
  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("for j in 0..m -> &p[i * m + j] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __xmodifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
}

void inner_alloc(int n, int m) {
  __pure();
  int* const p = (int*)calloc(n * m, sizeof(int));
  for (int i = 0; i < n; i++) {
    __strict();
    __xmodifies("for j in 0..m -> &p[i * m + j] ~> Cell");
    for (int j = 0; j < m; j++) {
      __strict();
      __xmodifies("&p[i * m + j] ~> Cell");
      p[i * m + j] = p[i * m + j] + i + j;
    }
  }
  free(p);
}

void copy(int* src, int* dest) {
  __writes("for i1 in 0..25 -> &dest[i1] ~> Cell");
  __reads("for i1 in 0..10 -> for i2 in 0..25 -> &src[25 * i1 + i2] ~> Cell");
  const __ghost_fn f1 = __ghost_begin(ro_group_focus, "i := 2, range := 0..10");
  const __ghost_fn f2 = __ghost_begin(
      ro_mindex2_unfold,
      "H := fun access -> for i in 0..25 -> access(2, i) ~> Cell");
  MATRIX1_COPY_int(dest, &src[2 * 25], 25);
  __ghost_end(f2);
  __ghost_end(f1);
}
