
int g(int a, int b) {
  return a + b + a;
}

int f() {
  const int x = 3;
  int y = 5;
  int z = 3;
  return g(x, g(y, z));
}

void h() {
  int a = g(f(), f());
}


int main() {
  f();

}
