void h(int x) {}

void g(int x) {
  h(x);
}

int f(int & a, const int b) {
  if (a > 2) {
    g(a);
    g(a);
    return a + 3;
  }

  g(a);
  g(b);

  return a + b;
}

int main () {
  int a = 2;
  int b = f(a, a);
  b = f(a, b);
  return 0;
}