
int f(int a, int b) {
  return a*b;
}

int main() {
  const int x = 1;
  const int y = 2;

  // folding 's1', which is a constant arithmetic expression
  const int r1 = f(x*y, 2*(x*y)); // don't know if it would work with 2*x*y or x*y*2 or none

  // folding 's2', which is a non-constant arithmetic expression
  int r2 = f(y*x, 2*(y*x));

  // folding 'a', which is a non-constant function call
  int a = f(2,2);
  int r = f(f(2,2),f(2,2));

}
