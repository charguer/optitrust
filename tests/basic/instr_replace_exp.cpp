int f(int x) {
  int y = 1;
  return y + x;
}

int f1(int x) {
  int y = -1;
  return y + x;
}

int f2(int a, int b, int c) { return a; }

int main() {
  int a = 5;
  int i = 10;
  int b = i;
  i = f(i);
  int y = 5;
  f(5);
  int d = f2(3, 2, 3);
  int e = f2(1, 1, 1);
  return 0;
}
