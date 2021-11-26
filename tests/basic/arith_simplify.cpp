int f(int x) {
  return x;
}

int main(int argc, char const *argv[])
{
  double a, b, c, d, e;
  double x, y, z, t, u, v, w;

  // test parsing and normalizing
  x = 2 * b;
  x = a + 2 * b - 3 * c;
  x = a + ((a + 2*b) + 1);
  x = 3 + 5.0 + a + (a + b*2 - 3*c);
  x = 3 * 5.0 * a * b * b;
  x = a / (b * c) / d;

  // test gather sum
  y = 1 + 2; // = 3
  y = 1 + 3.2 + 4 + 5.2; // = 5 + 3.2 + 5.2
  y = a + a + a - a; // = 2*a
  y = 2*a - 3*a + b - 5*b; // = -4*b - a
  y = 3*a + 2*b - b + c - 4*a ; // = b + c - a

  // test gather prod
  z = a * a * a / a; // = a * a
  z = a / b / a * b / b; // = 1 / b

  // test nested
  t = (a + 5*a);
  t = (a + (3 + 2)*a);
  t = a + b * a / b; // = 2 * a
  t = (a + (3 + 2)*a) / a * b; // = 6 * b

  // test expand
  u = a * (b + c); // = a * b + a * c
  u = (a + b) * (c + d); // = a * c + a * d + b * c + b * d
  u = 5 * a * (2*b + 3*c + 4*d);
  u = a / (b * a * b) / b; // = b

  // test from pic demo
  v = (a / b * c / (a * d * d / b * e)); // = d * d * c * e
  v = (a * b * c / a); // = b * c
  v = ((a / (b / c)) + d * c / b) * (b / c); // = a + d

  // test for recursion in atoms
  w = a + a * 3 + f(b * f(c + c) / b);
  return 0;
}

