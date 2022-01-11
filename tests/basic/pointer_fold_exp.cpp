int main() {
  int x = 2;
  int *y = (&x);
  int r1 = ((*y) + (*y));
  x = 9;
  int t[2] = {4, 5};
  int *a = (&t[1]);
  int r2 = ((*a) + (*a));
  (*a) = 9;
  int m[3][3];
  int *b = (&m[1][1]);
  int r3 = ((*b) + (*b));
  (*b) = 9;
  const int *v = m[0];
  int r4 = m[0][2];
  m[0][1] = 9;
}
