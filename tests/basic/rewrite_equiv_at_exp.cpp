int min(int a, int b) { return (a < b ? a : b); }

int main() {
  int x = 1;
  int y = 2;
  int z = 3;
  int res;
  res = z * y + x;
  int res1;
  res1 = 9 * 5;
  const int res2 = 9 * 5;
  const int res3 = res2;
  const int res4 = (9 + 1) * 5 + 3;
  const int res5 = 0;
  return 0;
}
