#include <optitrust.h>

void test_var() {
  __pure();
  double x = 0. * 5.;
  __ghost(to_prove, "P := __is_true(3. != 0)");
  const double z = 1. * 3.;
  x = (1. + z / 3.) * 5.;
  double y = x / 5. + 1.;
  x = (x / 5. + 2.) * 5.;
}

void test_array(double* t) {
  __modifies("&t[0] ~> Cell");
  __modifies("&t[1] ~> Cell");
  __reads("&t[2] ~> Cell");
  __ghost(to_prove, "P := __is_true(5. != 0)");
  t[0] = t[0] * 5.;
  t[0] = (t[0] / 5. + 1. + t[2]) * 5.;
  t[0] = t[0] / 5.;
}
