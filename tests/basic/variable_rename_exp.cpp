int f(int x) {
  int y = 1;
  int z = 2;
  return (y + z);
}

int g(int x) {
  int y = (-1);
  return (y + x);
}

int main() {
  int a1 = 1;
  int b1 = f(a1);
  int c = g(b1);
  return 0;
}
