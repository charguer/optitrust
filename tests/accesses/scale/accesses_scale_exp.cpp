void test_var() {
  double x = 0. * 5.;
  x = 1. * 5.;
  double y = x / 5. + 1.;
  x = (x / 5. + 2.) * 5.;
}

void test_array() {
  double t[2] = {1., 2};
  t[0] = t[0] + 1.;
}

int main() {
  double t[3] = {1., 2., 3.};
  double v = 2.;
  int i = 0;
  t[i] = t[i] + v;
  return 0;
}
