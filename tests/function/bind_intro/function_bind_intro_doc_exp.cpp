int f(int x) { return x + 1; }

int g(int x) { return x + 1; }

int main() {
  const int a = g(1);
  int b = f(a);
}
