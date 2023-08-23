inline int min(int a, int b) { return a < b ? a : b; }

int main() {
  int x = 1;
  int y = 2;
  int z = 3;
  int res;
  res = x + y * z ;
  int res1;
  res1 = 5 + 8 * 5;
  const int res2 = 5 + 8 * 5;
  const int res3 = (res2 + 0) + 0;
  const int res4 = 5 + (5 + 8 * 5) + 3;
  const int res5 = min(0, 1);
  return 0;
}