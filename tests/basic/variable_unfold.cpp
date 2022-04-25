int f(int a) {
  return a + 1;
}

int main() {
  // inlining without removal of 'a'
  const int a = 2;
  const int b = a + a;

  
  int c = 3;
  int d = c + c;
  
  
  int& e = c;
  d = e + e;


  // inlining of function
  const int e1 = f(2);

  

  return 0;
}
