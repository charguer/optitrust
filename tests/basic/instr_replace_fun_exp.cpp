int f(int x) {
  int y = 1;
  return (y + x);
}

int f1(int x) {
  int y = (-1);
  return (y + x);
}

int f2(int x, int y, int z) { return ((x + y) + z); }

int f3(int x, int y, int z) { return ((x - y) - z); }

int main() {
  int a = 5;
  a = f1(a);
  int c = f(4);
  int d = f3(1, 2, 3);
  int e = f2(1, 2, 2);
  return 0;
}
