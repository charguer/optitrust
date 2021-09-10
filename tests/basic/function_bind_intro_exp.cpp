int f(int x) { return (x + 1); }

int g(int x, int y) { return (x + y); }

int h() { return 1; }

int main() {
  int a = 3;
  int r = g(a, 4);
  int t = f(r);
  int s = h();
  int z = f(s);
  return 0;
}