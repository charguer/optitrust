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

  int x = 0;
  for (int i = 0; i < N-2; i++) {
    __ghost([&] {
      __requires("in_range(i, 0..(N-2))");
    }, "");
    x += i;
  }
}

void arrays(int N, int* w, int* r, int* f) {
  __writes("w ~> Matrix1(N)");
  __reads("r ~> Matrix1(N)");
  __modifies("f ~> Matrix1(N)");

  for (int i = 0; i < N; i++) {
    __xwrites("&w[MINDEX1(N, i)] ~> Cell");
    // FIXME: __xreads("&r[MINDEX1(N, i)] ~> Cell");
    __xmodifies("&f[MINDEX1(N, i)] ~> Cell");
    w[MINDEX1(N, i)] = i; /* FIXME: + r[MINDEX1(N, i)]; */
    f[MINDEX1(N, i)] = i + f[MINDEX1(N, i)];
  }
}
