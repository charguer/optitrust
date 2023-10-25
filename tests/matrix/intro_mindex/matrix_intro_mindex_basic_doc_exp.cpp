int main() {
  const int N = 5;
  int p[5] = {0, 1, 2, 3, 4};
  p[MINDEX1(N, 0)] = 10;
  int a = p[MINDEX1(N, 1)];
  return 0;
}
