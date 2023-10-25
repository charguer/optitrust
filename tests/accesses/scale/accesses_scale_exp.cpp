const int N = 5;

double t[N];

void test_var() {
  double x = 0.;
  x = 1. * 5.;
  double y = x / 5. + 1.;
  x = (x / 5. + 2.) * 5.;
}

void test_array() {
  double t[2] = {1., 2};
  t[0] = (t[0] / 5. + 1.) * 5.;
}

int main() {
  double t[3] = {1., 2., 3.};
  double v = 2.;
  int i = 0;
  t[i] = (t[i] / 5. + v) * 5.;
  return 0;
}
