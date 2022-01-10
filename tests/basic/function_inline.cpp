int f(int x) {
  int a = x + x;
  return a + a;
}

int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

int h(int x) {
  if (x > 0)
    return 1;
  return 2;
}

void m(int* p) {
  (*p)++;
}

int k(int a, int b) {
  return a + b;
}

int main(){
  int x = 3;
  int y = f(x);
  int z = g(x);
  int u = h(x);
  int *q = new int(3);
  m(q);
  int result = k(result, 4);
  return 0;
}
