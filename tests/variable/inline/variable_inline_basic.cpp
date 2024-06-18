int f(int a) {
  return a + 1;
}

int main() {

  // inlining of variable
  const int a = 2;
  const int b = a + a;

  int c = 3;
  int d = c;
  c = d;
  // inlining of function
  const int e = f(2);

  return 0;
}

/* TODO:

int y = f(&x); // can't inline
for i in r:
  pre/post uninit x

  x = 2;
  y;
  g(x);


also tests where instrs cannot be deletete or duplicated through control flow

*/
