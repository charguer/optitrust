int f(int x) { return x + 1; }

int f1() { f(1); }

int main() {
  int a;
  a = f1();
  return 0;
}
