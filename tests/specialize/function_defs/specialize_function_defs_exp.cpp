int f(int a, int b) { return a + b; }

int f4() { f(2, 3); }

int f3(int b) { f(3, b); }

int f2(int a) { f(a, 3); }

int f1(int a, int b) { f(a, b); }

int main() {
  int x;
  x = f(x, x + 1);
  return 0;
}
