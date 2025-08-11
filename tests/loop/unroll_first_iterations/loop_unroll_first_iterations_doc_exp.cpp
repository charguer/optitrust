#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  __ghost(to_prove, "P := is_subrange(0..2, 0..10)");
  __ghost(assume, "P := in_range(0, 0..10)");
  x += 0;
  __ghost(assume, "P := in_range(1, 0..10)");
  x += 1;
  for (int i = 2; i < 10; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
}
