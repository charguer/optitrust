int *t;

int main() {
  int N = 10;
  for (int c = 0; (c < 2); c++) {
    for (int i = c; (i < N); i = (i + 2)) {
      t[i] = 0;
    }
  }
  for (int c = 0; (c < 2); c++) {
    for (int j = (c * 2); (j < N); j = (j + (2 * 2))) {
      t[j] = 0;
    }
  }
  return 0;
}
