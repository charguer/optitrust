#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 4; i_s < 12; i_s++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += i_s;
  }
  for (int i2 = 2; i2 < 12; i2++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += i2 - 2;
  }
  int w = 10 + 2;
  for (int j2 = 0; j2 < N; j2++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += j2 + st;
  }
  const int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    __strict();
    __smodifies("&x ~> Cell");
    const int k = k2 - shift;
    x += k;
  }
  for (int m = 8; m < N + 4; m++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += m - 6;
  }
}
