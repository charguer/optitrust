int f(int x) { return x + 1; }

int main() {
  int a = 1;
  int *b = &a;
  int *c;
  c = b;
  int d;
  d = a;
  int t[2] = {1, 2};
  int *e = &t[0];
}
