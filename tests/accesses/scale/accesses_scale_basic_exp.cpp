#include <optitrust.h>

void f() {
  double t[3] = {1., 2., 3.};
  double v;
  v = 2.;
  __ghost(to_prove, "P := __is_true(0.5 != 0)");
  const double u = 1. * 0.5;
  int i = 0;
  t[i] = t[i] + v * (u / 0.5);
}
