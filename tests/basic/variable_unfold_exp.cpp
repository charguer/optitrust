int main() {
  int const a = 2;
  int const b = (2 + 2);
  int const e = (int f(int a) { return (a + 1); })(2);
  return 0;
}
