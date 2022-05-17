
int g(int a, int b) {
  return a + b;
}

int f() {
  const int x = 3;
  int y = 5;
  int z = 3;
  g(x, g(y, z));
}


int main() {
  f();

}