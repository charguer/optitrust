int f(int a, int b) { return (a + b); }

int main(int argc, char const *argv[]) {
  int x = 0;
  x = f(5, x);
  x += 3;
  x += 3;
  x -= 2;
  return 0;
}
