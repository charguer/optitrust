int f(int x) {
  int a = x + x;
  return a + a;
}

int g(int x){
  const int b = f(x);
  return b;
}

int main() {
  int x = 3;
  const int y = f(x);
  return 0;
}
