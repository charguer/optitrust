#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  __ghost(to_prove, "P := is_subrange(0..(0 + 2), 0..10)");
  for (int i = 0; i < 0 + 2; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  for (int i = 0 + 2; i < 10; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  const int cut = 2;
  __ghost(to_prove, "P := is_subrange(0..cut, 0..10)");
  for (int k = 0; k < cut; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(k, 0..10)");
    x += k;
  }
  for (int k = cut; k < 10; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(k, 0..10)");
    x += k;
  }
}
