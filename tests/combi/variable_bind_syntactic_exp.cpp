int main() {
  int s = 0;
  int a = 2 + 3;
  for (int i = 0; i < 3; i++) {
    s = a;
  }
  int t = 0;
  int b = 2 + s;
  for (int j = 0; j < 3; j++) {
    t += b;
    t += b;
  }
  int c0 = 4 + 3;
  int c1 = 4 + 4;
  int r = c0 * c0 + c1 * c1;
  return 0;
}
