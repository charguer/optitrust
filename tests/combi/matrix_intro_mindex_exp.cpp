int main() {
  const int N = 5;
  int p[5] = {0, 1, 2, 3, 4};
  for (int i = 1; i < 5; i++) {
    int a = p[MINDEX1(N, i - 1)] + 2;
    p[MINDEX1(N, i)] = a;
  }
  return 0;
}
