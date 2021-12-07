int main() {
  int x = 2;
  int const &y = x;
  int r1 = (x + x);
  int t[2] = {4, 5};
  int r2 = (t[1] + t[1]);
  int m[3][3];
  int const &b = m[1][1];
  int r3 = (m[1][1] + m[1][1]);
}
