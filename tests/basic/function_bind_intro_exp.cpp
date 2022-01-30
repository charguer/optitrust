int f(int x) { return x + 1; }

int g(int x, int y) { return x + y; }

int h() { return 1; }

int main() {
  int a = 3;
  const int r = g(a, 4);
  int t = f(r);
  const int b = f(a);
  int u = f(b);
  const int s = h();
  int z = f(s);
  return 0;
}
