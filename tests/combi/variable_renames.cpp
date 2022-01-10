int f(int x) {
  int y = 1;
  int z = 2;
  return y+z;
}

int g(int x) {
  int y = -1;
  return y+x;
}

int main() {
  int a = 1;
  int b = f(a);
  int c = g(b);
  return 0;
}