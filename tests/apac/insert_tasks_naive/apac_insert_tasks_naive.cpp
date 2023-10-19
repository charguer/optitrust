int f(int a, int b) {
  return a + b;
}

int g(int &a) {
  return 0;
}

int h() {
  int a;
  int b;
  a = 1;
  b++;
  g(b);
  a = 2;
  f(a, b);
  a = 3;
}