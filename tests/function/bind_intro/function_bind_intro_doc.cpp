
int f(int x) { return (x + 1); }

int g(int x) { return (x + 1); }

int main() {
  int b = f(g(1));
}
