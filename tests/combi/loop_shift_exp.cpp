int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 4; i_s < 12; i_s++) {
    x += i_s;
  }
  for (int i2 = 2; i2 < 12; i2++) {
    x += i2 + -2;
  }
  int w = 10 + 2;
  for (int j2 = 0; j2 < N; j2++) {
    x += j2 + st;
  }
  int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    const int k = k2 - shift;
    x += k;
  }
}
