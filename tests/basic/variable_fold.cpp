
int f(int a, int b) {
  return a*b;
}

int main() {
  const int x = 1;
  const int y = 2;

  // folding 's1', which is a constant arithmetic expression
  const int s1 = x*y;
  const int r1 = f(x*y, 2*(x*y)); // don't know if it would work with 2*x*y or x*y*2 or none

}
