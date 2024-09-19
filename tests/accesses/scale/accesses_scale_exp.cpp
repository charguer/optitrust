#include <optitrust.h>

void test_var() {
  __pure();
  double x = 0. * 5.;
  x = 1. * 5.;
  double y = x / 5. + 1.;
  x = (x / 5. + 2.) * 5.;
  /*@ __3__begin, __3__end @*/
}

void test_array(double* t) {
  __modifies("&t[0] ~> Cell");
  __modifies("&t[1] ~> Cell");
  /*@ __16__begin @*/
  t[0] = t[0] * 5.;
  /*@ __16__end @*/
  t[0] = (t[0] / 5. + 1.) * 5.;
  /*@ __17__begin @*/
  t[0] = t[0] / 5.;
  /*@ __17__end @*/
}
