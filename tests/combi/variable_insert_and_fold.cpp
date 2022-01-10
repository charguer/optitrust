
int f(int a, int b) {
  return a*b;
}

int main() {
  const int x = 1;
  const int y = 2;

  const int r1 = f(x*y, 2*(x*y)); // don't know if it would work with 2*x*y or x*y*2 or none

  int r2 = f(y*x, 2*(y*x));

  int r = f(f(2,2),f(2,2));

}
