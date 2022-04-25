int f(int a) { return a + 1; }

int main() {
  const int a = 2;
  const int b = 2 + 2;
  int c = 3;
  int d = c + c;
  int& e = c;
  d = c + c;
  const int e1 = (int f(int a) { return a + 1; })(2);
  return 0;
}
