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

  int* const p = CALLOC2(int, n, m);
  for (int i = 0; i < n; i++){
    __xmodifies("for j in 0..m -> &p[MINDEX2(n, m, i, j)] ~> Cell");

    for (int j = 0; j < m; j++){
      __xmodifies("&p[MINDEX2(n, m, i, j)] ~> Cell");

      p[MINDEX2(n, m, i, j)] = p[MINDEX2(n, m, i, j)] + i + j;
    }
  }
  free(p);
}

void copy(int* src, int* dest) {
  __reads("src ~> Matrix2(10, 25)");
  __writes("dest ~> Matrix1(25)");

  __GHOST_BEGIN(f1, ro_group_focus, "2, 0..10");
  __GHOST_BEGIN(f2, ro_mindex2_unfold, "H := fun access -> for i in 0..25 -> access(2, i) ~> Cell");
  MATRIX1_COPY_int(dest, &src[2*25], 25);
  __GHOST_END(f2);
  __GHOST_END(f1);
}
