#include <optitrust.h>

void f(int N) {
  __pure();

  int x = 0;

  for (int i = 0; i < 10; i++) {
    x += i * 2;
  }

  const int ratio = 5;
  for (int j = 0; j < N; j++) {
    x += j * ratio;
  }
}

void ghost_in_range(int N) {
  __pure();

  int x;
  for (int i = 0; i < N-2; i++) {
    __ghost([&] {
      __requires("in_range(i, 0..(N-2))");
    }, "");
    x += i;
  }
}

void array(int N, int* p) {
  __writes("p ~> Matrix1(N)");

  for (int i = 0; i < N; i++) {
    __xwrites("&p[MINDEX1(N, i)] ~> Cell");
    p[MINDEX1(N, i)] = i;
  }
}
