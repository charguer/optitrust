int f(int x) {
  return x+1;
}

int g(int x, int y) {
  return x + y;
}

int h() {
  return 1;
}

int main() {
  int a = 3;
  int t = f(g(a,4));
  int u = f(f(a));
  int z = f(h());
  return 0;
}
