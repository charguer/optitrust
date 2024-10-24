#include <optitrust.h>

void outer_alloc(int n, int m, int* p) {
  __modifies("p ~> Matrix2(n, m)");

  for (int i = 0; i < n; i++){
    __xmodifies("for j in 0..m -> &p[MINDEX2(n, m, i, j)] ~> Cell");

    for (int j = 0; j < m; j++){
      __xmodifies("&p[MINDEX2(n, m, i, j)] ~> Cell");

      p[MINDEX2(n, m, i, j)] = p[MINDEX2(n, m, i, j)] + i + j;
    }
  }
}

void inner_alloc(int n, int m) {
  __pure();

  int* const p = (int*) CALLOC2(n, m, sizeof(int));
  for (int i = 0; i < n; i++){
    __xmodifies("for j in 0..m -> &p[MINDEX2(n, m, i, j)] ~> Cell");

    for (int j = 0; j < m; j++){
      __xmodifies("&p[MINDEX2(n, m, i, j)] ~> Cell");

      p[MINDEX2(n, m, i, j)] = p[MINDEX2(n, m, i, j)] + i + j;
    }
  }
  MFREE2(n, m, p);
}

void copy(int* src, int* dest) {
  __reads("src ~> Matrix1(10)");
  __modifies("dest ~> Matrix1(25)");

  __GHOST_BEGIN(f1, group_focus_subrange, "15..25, 0..25");
  MMEMCPY(dest, 15, src, 0, 10, sizeof(int));
  __GHOST_END(f1);
}

void copy_void(void* src, void* dest) {
  __reads("src ~> Matrix1(10)");
  __modifies("dest ~> Matrix1(25)");

  __GHOST_BEGIN(f1, group_focus_subrange, "15..25, 0..25");
  MMEMCPY(dest, 15, src, 0, 10, sizeof(int));
  __GHOST_END(f1);
}
