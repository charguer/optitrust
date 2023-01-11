int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i_s = 2; i_s < 10 + 2; i_s++) {
    x += i_s - 2;
  }
  for (int i2 = 2; i2 < 10 + 2; i2++) {
    x += i2 - 2;
  }
  for (int j2 = 0; j2 < N; j2++) {
    x += j2 + st;
  }
  int shift = 5;
  for (int k2 = shift; k2 < N + shift; k2++) {
    const int k = k2 - shift;
    x += k;
  }
}
