int f(int a, int b) { return (a * b); }

int main() {
  const int x = 1;
  const int y = 2;
  const int s1 = (x * y);
  const int r1 = f(s1, (2 * s1));
}
