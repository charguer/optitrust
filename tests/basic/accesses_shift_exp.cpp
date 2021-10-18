int const N = 5;

double t[N];

int main() {
  double t[3] = {1., 2., 3.};
  double v = 2.;
  int i = 0;
  t[i] = (((((((t[i] / 6.) - 6.) / 5.) + v) * 5.) + 6.) * 6.);
  return 0;
}