void g(int x, int y) {}


void f(int x) {
  int a = x+1;
  g(a, x);
}


int main() {
  int r = 5;
  int b = (r+2)+1;
  g(b, r+2);
  for (int i = 0; i < 2; i++) {
    int c = r+1;
    g(c,r);
  }
  int s = r;
  return 0;
}
