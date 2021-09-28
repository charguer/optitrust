int f(int a, int b) { return (a * b); }

int main() {
  int const x = 1;
  int const y = 2;
  int s1 = (x * y);
  int const r1 = f(s1, (2 * s1));
  int s2 = (y * x);
  int r2 = f(s2, (2 * s2));
  int s3 = f(2, 2);
  int r = f(s3, s3);
}