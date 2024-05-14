int main() {
  int x = 0;
  const int st = 0;
  const int N = 10;
  x += 0;
  for (int i = 1; i < 10; i++) {
    x += i;
  }
  x += st;
  for (int j = st + 1; j < N; j++) {
    x += j;
  }
  int cut = 5;
  x += 0;
  for (int k = 1; k < N; k++) {
    x += k;
  }
  x += st;
  for (int l = st + 1; l < N; l++) {
    x += l;
  }
}
