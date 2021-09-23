int f(int x) { return (x + 1); }

int g(int x, int y) { return (x + y); }

int h() { return 1; }

int main() {
  int a = 3;
  int t = f(r);
  int r = g(a, 4);
  int z = f(s);
  int s = h();
  return 0;
}