int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

void test_const_ret() {
  int x = 3;
  const int y = 10;
  const int z = g(x);
  int s = y + z;
}
