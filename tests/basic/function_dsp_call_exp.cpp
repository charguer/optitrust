int f(int x) {
  if (x > 0) {
    return x;
  } else {
    return -x;
  }
}

void f_dsp(int x, int* res) {
  if (x > 0) {
    *res = x;
  } else {
    *res = -x;
  }
}

int g(int x, int y) {
  if (x > 0) {
    return x + y;
  } else {
    return -x + y;
  }
}

void g_dsp(int x, int y, int* res) {
  if (x > 0) {
    *res = x + y;
  } else {
    *res = -x + y;
  }
}

int h() { return 3; }

void my_h(int* res) { *res = 3; }

void test_one_arg() {
  int r;
  int x = 1;
  f_dsp(x, &r);
}

void test_two_args() {
  int r;
  int x = 1;
  int y = 2;
  g_dsp(x, y, &r);
}

void test_zero_arg() {
  int r;
  my_h(&r);
}

int main() { return 0; }
