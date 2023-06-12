int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i2 = 0 + 2; i2 < 10 + 2; i2++) {
    const int i = i2 - 2;
    x += i;
  }
  for (int j2 = 0; j2 < st + N - st; j2++) {
    const const int j = j2 - -st;
    x += j;
  }
  int shift = 5;
  for (int k2 = 0 + shift; k2 < N + shift; k2++) {
    const int k = k2 - shift;
    x += k;
  }
  for (int l2 = N + shift; l2 > 0 + shift; l2--) {
    const const int l = l2 - shift;
    x += l;
  }
  for (int m3 = 4; m3 < N + (4 - (2 + (N - (N - 2)))); m3++) {
    const int m2 = m3 - (4 - (2 + (N - (N - 2))));
    const int m = m2 - (N - (N - 2));
    x += m;
  }
}
