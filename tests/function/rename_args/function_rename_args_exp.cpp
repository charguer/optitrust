void test_no_args() {
  int z = 10;
  z = 20;
}

void test_one_arg(int x1) {
  int z = x1;
  z = 10;
}

void test_two_args(int x2, int y2) {
  int z = x2;
  for (int x = 0; x < 4; x++) {
    z += x + y2;
    int y = 1;
    int r = y + x;
  }
  int a = x2 + y2;
}
