int f(int x) { return x; }

int g(int x) { return x + 1; }

void simpl_in_depth() {
  int x;
  x = g(1 + 2 + f(2 + 4)) + g(5) + g(5);
}

int main() {
  double a;
  double b;
  double c;
  double d;
  double e;
  double r;
  double s;
  double t;
  double u;
  double v;
  double w;
  double x;
  double y;
  double z;
  x = 2 * b;
  x = a + 2 * b - 3 * c - d;
  x = a + a + 2 * b + 1;
  x = 3 + 5. + a + a + 2 * b - 3 * c;
  x = 3 * 5. * a * b * b;
  x = a / b / b / c / d;
  y = 3;
  y = 5 + 3.2 + 5.2;
  y = 2 * a;
  y = -a - 4 * b;
  y = -a + b + c;
  z = a * a;
  z = 1. / b;
  t = 6 * a;
  t = 2 * a;
  t = 6 * b;
  t = 1. / b;
  u = a * b + a * c;
  u = a * c + a * d + b * c + b * d;
  u = 5 * a * 2 * b + 5 * a * 3 * c + 5 * a * 4 * d;
  v = c / (d * d) / e;
  v = b * c;
  v = a + d;
  v = d;
  v = d;
  v = a + c;
  w = a + a * 3 + f(b * (f(c + c)) / b);
  return 0;
}
