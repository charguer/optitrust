int g(int a, int b) { return a + b + a; }

int f(int x) {
  int y = 5;
  int z = 3;
  return y + z;
}

void test_invariant1(int x) {
  int a;
  a = f(x);
  f(x);
}

void test_invariant2(int x) { const int c = f(x); }

void test_detach(int x) {
  int d;
  d = f(x);
}

void test_expression(int x) {
  int res__0;
  res__0 = f(x);
  int res__2;
  res__2 = g(0, 1);
  int b = res__0 + res__2;
}

void test_nested_call(int x) {
  int res__0;
  res__0 = f(x);
  int e;
  e = g(res__0, x);
}

void test_not_indepth(int x) {
  int res__0;
  res__0 = f(x);
  int z = g(res__0, x);
}

int main() {}
