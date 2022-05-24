void test_decl() {
  int b = 2;
  int d = 6;
  {
    int a;
    int c = 5;
    int e = 7;
  }
}

void test_loop() {
  int a = 0;
  for (int i = 0; i < 10; i++) {
    i = i + 1;
  }
  { int g = 10; }
}

int main() { return 0; }
