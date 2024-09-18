#include <optitrust.h>

void test_var() {
  __pure();

  double x = 0.0;
  x = 1.0;
  double y = x + 1.0;
  x = x + 2.0;
}

void test_array(double* t) {
  __modifies("&t[0] ~> Cell, &t[1] ~> Cell");
  t[0] = t[0] + 1.0;
}
/*
int main() {
  __pure();

  double t[3] = { 1., 2., 3. };
  double v = 2.0;
  int i = 0;
  t[i] = t[i] + v;
  return 0;
}
*/
