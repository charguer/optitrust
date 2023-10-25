int f(int x) { return x + 1; }

int g(int x, int y, int z, int w) {
  int p = x + y + z + w;
  return p + p;
}

int h(int x) { return x - 1; }

int m(int x, int y) { return x - y; }

int main() {
  int u, v, w;
  int a = h(4);
  int b = m(v, 2);
  int t = g(a, u, b, w + 1);
  return 0;
}

void main2() {
  int u, v, w;
  int a = h(4);
  int b = m(v, 2);
  int c = w + 1;
  int t = f(g(a, u, b, c));
}
