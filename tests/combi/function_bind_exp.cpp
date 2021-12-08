int f(int x) { return (x + 1); }

int g(int x, int y, int z, int w) {
  int p = (((x + y) + z) + w);
  return (p + p);
}

int h(int x) { return (x - 1); }

int m(int x, int y) { return (x - y); }

int main() {
  int u = 1, v = 2, w = 3;
  int a = h(4);
  int b = m(v, 2);
  int r = g(a, u, b, (w + 1));
  int t = f(r);
  return 0;
}
