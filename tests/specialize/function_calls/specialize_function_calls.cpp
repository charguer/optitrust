int f(int a, int b) { return a + b; }

int f1(int a, int b) { return f(a, b); }

int f2(int a) { return f(a, 3); }

int f3(int b) { return f(3, b); }

int f4() { return f(2, 3); }


int main() {
  int x;
  x = f(x, x + 1);

  x = f(x, 3);

  x = f(3, x);

  x = f(2, 3);

  return 0;

}
