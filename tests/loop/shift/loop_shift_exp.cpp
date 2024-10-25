#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 4; i_s < 12; i_s++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(i_s - 2, 2..10)");
    x += i_s;
  }
  for (int i2 = 2; i2 < 12; i2++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(i2 - 2, 0..10)");
    x += i2 - 2;
  }
  int w = 10 + 2;
  for (int j2 = 0; j2 < N; j2++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(j2 + st, st..st + N)");
    x += j2 + st;
  }
  const int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int k = k2 - shift;
    __ghost(assume, "F := in_range(k, 0..N)");
    x += k;
  }
  for (int m = 8; m < N + 4; m++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "F := in_range(m - 4, 4..N)");
    __ghost(assume, "F := in_range(m - 6, 2..N - 2)");
    __ghost([&]() { __requires("in_range(m - 6, 2..N - 2)"); }, "");
    x += m - 6;
  }
}
