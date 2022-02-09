int f(int a) {
  return a + 1;
}

int main() {
  // inlining without removal of 'a'
  const int a = 2;
  const int b = a + a;

  
  int c = 3;
  int r = c + c;
  
  // inlining of function
  const int e = f(2);

  

  return 0;
}

