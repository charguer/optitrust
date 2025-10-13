#include <optitrust.h>

void test_var() {
  __pure();

  double x = 0.0;
  x = 1.0;
  double y = x * 1.0;
  x = x * 2.0;
}

void test_var_inv(int* t, int n) {
  __reads("t ~> Matrix1(n)");

  int s = 0;
  for (int i = 0; i < n; i++) {
    __spreserves("&s ~> Cell");
    s = s + 1;
  }
}


void test_array() {
  double t[2] = { 1., 2 };
  t[0] = t[0] * 1.0;
}


int main() {
   double t[3] = { 1., 2., 3. };
   double v = 2.0;
   int i = 0;
   t[i] = t[i] * v;
   return 0;
}

