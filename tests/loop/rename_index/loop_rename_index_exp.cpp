int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i2 = 0; i2 < 10; i2++) {
    x += i2;
  }
  for (int j2 = st; j2 < st + N; j2++) {
    x += j2;
  }
  int shift = 5;
  for (int foo = 0; foo < N; foo++) {
    x += foo;
  }
  for (int bar = N; bar > 0; bar--) {
    x += bar;
  }
}
