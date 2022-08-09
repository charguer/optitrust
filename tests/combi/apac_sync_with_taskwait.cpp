
int f() {
  int a;
  int b;
  if (a > b) {
    int c = a + b;
    while (c > a) {
      c++;
    }
  }
  return a;
}