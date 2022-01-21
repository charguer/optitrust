int f(int a) { return (a + 1); }

int main() {
  const int a = 2;
  const int b = (2 + 2);
  const int e = (int f(int a) { return (a + 1); })(2);
  return 0;
}
