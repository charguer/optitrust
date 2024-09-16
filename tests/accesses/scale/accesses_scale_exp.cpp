#include <optitrust.h>

void test_var() {
  __pure();
  double x = 0. * 5.;
  x = 1. * 5.;
  double y = x / 5. + 1.;
  x = (x / 5. + 2.) * 5.;
  /*@ __3__begin, __3__end @*/
}
