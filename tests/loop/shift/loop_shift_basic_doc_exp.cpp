#include <optitrust.h>

int main() {
  int x = 0;
  for (int i2 = 0; i2 < 12 - 2; i2++) {
    const int i = i2 - -2;
    __ghost(assume, "F := in_range(i, 2..12)");
    x += i;
  }
  const int shift = 2;
  for (int k2 = 0 + shift; k2 < 10 + shift; k2++) {
    const int k = k2 - shift;
    __ghost(assume, "F := in_range(k, 0..10)");
    x += k;
  }
}
