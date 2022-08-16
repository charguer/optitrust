int f(int i) {
  return i;
}

int g(int i, int j) {
  return i+j;
}

void h() {
  const int a = f(1);
  int b;
  b = f(f(1));
  int c = g(f(1), 1);
}