int main() {
  int const N = 5;
  int p[5] = {0, 1, 2, 3, 4};
  for (int i = 0; (i < 5); i++) {
    p[MINDEX1(N, i)] = 0;
  }
  return 0;
}
