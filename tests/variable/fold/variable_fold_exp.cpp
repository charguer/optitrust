int f(int a, int b) { return a * b; }

int main() {
  const int x = 1;
  const int y = 2;
  const int s1 = x * y;
  const int r1 = f(s1, 2 * s1);
  int s2 = y * x;
  int r2 = f(s2, 2 * s2);
  int a = f(2, 2);
  int r = f(f(2, 2), f(2, 2));
}
