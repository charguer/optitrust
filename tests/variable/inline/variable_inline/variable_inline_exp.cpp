int main() {
  const int b = 2 + 2;
  int c = 3;
  int d = c;
  c = d;
  const int e = (int f(int a) { return a + 1; })(2);
  return 0;
}
