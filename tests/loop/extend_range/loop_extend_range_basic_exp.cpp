int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i = 0; i < 10; i++) {
    if (2 <= i) {
      if (false) {
        x += i;
      }
    }
  }
  for (int j = st; j < N + 5; j++) {
    if (j < N) {
      x += j;
    }
  }
  int ld = 2;
  int u = N + 5;
  for (int k = 0 - ld; k < u; k++) {
    if (0 <= k && k < N) {
      x += k;
    }
  }
}
