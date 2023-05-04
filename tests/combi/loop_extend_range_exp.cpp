int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  for (int i = 0; i < 10; i++) {
    if (2 <= i) {
      x += i;
    }
  }
  for (int j = st; j < 20; j++) {
    if (j < 15) {
      x += j;
    }
  }
  int ld = 2;
  int u = N + 5;
  for (int k = -ld; k < u; k++) {
    if (0 <= k && k < N) {
      x += k;
    }
  }
  for (int l = 2; l < 4; l++) {
    if (3 <= l && l < 3) {
      x += l;
    }
  }
}
