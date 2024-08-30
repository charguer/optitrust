void f() {
  double t[3] = {1., 2., 3.};
  double v;
  v = 2. * 2.;
  const double u = 1. * 0.5;
  int i = 0;
  t[i] = t[i] + v / 2. * (u / 0.5);
}
