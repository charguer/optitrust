int f(int a, int b) { return a * b; }

int main() {
  const int x = 1;
  const int y = 2;
  const int s1 = x * y;
  const int r1 = f(s1, 2 * s1);
  const int s2 = y * x;
  int r2 = f(s2, 2 * s2);
  const int s3 = f(2, 2);
  int r = f(s3, s3);
}
