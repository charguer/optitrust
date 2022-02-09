int main() {
  int x = 2;
  int &y = x;
  y = 3;
  int r1 = x + x;
  int t[2] = {4, 5};
  t[1] = 3;
  int r2 = t[1] + t[1];
  int m[3][3];
  int &b = m[1][1];
  b = 3;
  int r3 = m[1][1] + m[1][1];
}
