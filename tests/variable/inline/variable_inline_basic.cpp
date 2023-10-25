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

