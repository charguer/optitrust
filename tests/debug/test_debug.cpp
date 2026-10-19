#include <optitrust_gpu.h>

void f(int *b) {
  __requires("B : int -> int");
  __reads("b ~> Matrix1(10, B)");
  int* const a = MALLOC1(int, 10);
  for (int i = 0; i < 10; i++) {
    __sreads("b ~> Matrix1(10, B)");
    __xwrites("&a[MINDEX1(10, i)] ~~> B(i)");
    __GHOST_BEGIN(focusb, ro_matrix1_focus, "b, i");
    a[MINDEX1(10,i)] = b[MINDEX1(10, i)];
    __GHOST_END(focusb);
  }
  free(a);
}
