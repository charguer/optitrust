#include <optitrust.h>

typedef int T;
T* b;
/*
void g() {
  __pure();

  const int N0 = 5;
  const int N1 = 10;
  const int N2 = 10;
  const int N3 = 10;
  T* const a = CALLOC3(T, N1, N2, N3);
  for (int i = 0; i < 10; i++) {
    __GHOST_BEGIN(focus, ro_matrix3_focus, "a, i, i+1, i+2");
    a[MINDEX3(N1,N2,N3,i,i+1,i+2)];
    __GHOST_END(focus);
  }
*//*
  b = (T*) CALLOC3 (N1, N2, N3, sizeof(T));
  for (int j = 0; j < 10; j++) {
    b[MINDEX3(N1,N2,N3,j,j+1,j+2)];
  }
*//*
  int z = 0;
}
*/
void f(int* c, int n) {
  __modifies("c ~> Matrix1(n)");

  for (int i = 0; i < n; i++) {
    __xmodifies("&c[MINDEX1(n, i)] ~> Cell");
    c[MINDEX1(n, i)] = i;
  }
}
