#include <optitrust.h>

void f() {
  int x = 0;
  for (int i2 = 0; i2 < 10; i2++) {
    __ghost(assume, "P := in_range(i2 + 2, 2..12)");
    x += i2 + 2;
  }
  const int shift = 2;
  for (int k = shift; k < 10 + shift; k++) {
    __ghost(assume, "P := in_range(k - shift, 0..10)");
    x += k - shift;
  }
}
