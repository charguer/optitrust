void f(int);

int test_const_const() {
  const int y = 5;
  int z = y + y;
}

int test_nonconst_const() {
  int y = 5;
  y = 6;
  y = 7;
  int z = y + y;
}

int test_const_nonconst() {
  const int y = 5;
  int z = y + y;
}

int test_nonconst_nonconst() {
  int y = 5;
  y = 6;
  y = 7;
  int z = y + y;
}
