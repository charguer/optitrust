
int f(int a, int b) {
  return a*b;
}

int main() {
  const int x, y;

  const int s1 = x*y;
  const int r1 = f(s1, 2*s1);

  int r2 = f(s2, 2*s2);

  int a = f(2,2);
  int r = f(a,a);

}
