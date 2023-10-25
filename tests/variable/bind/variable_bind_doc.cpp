
int f(int x);
int g(int x);

int main() {
  int a = 1;
  int r = g(f(a + 3));
}
