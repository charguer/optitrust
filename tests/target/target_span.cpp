int main() {
  int a = 0;
  int b = 1;

  for(int i = 0; i < 100; ++i) {
    a += 1;
    a += 2;
    b += 1;
    b += 2;
  }

  a += 1;
  a += 2;
}
