int f(int a, int b) { return a + b; }

int f4() { return f(2, 3); }

int f3(int b) { return f(3, b); }

int f2(int a) { return f(a, 3); }

int f1(int a, int b) { return f(a, b); }

int main() {
  int x;
  x = f1(x, x + 1);
  x = f2(x);
  x = f3(x);
  x = f4();
  return 0;
}
