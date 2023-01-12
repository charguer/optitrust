int f(int i) { return i; }

int g(int i, int j) { return i + j; }

void h() {
  int __var_1;
  __var_1 = f(1);
  const int a = __var_1;
  int b;
  int __var_2;
  __var_2 = f(1);
  b = f(__var_2);
  int __var_3;
  __var_3 = f(1);
  int __var_4;
  __var_4 = f(2);
  int __var_5;
  __var_5 = g(__var_3, __var_4);
  int c = __var_5;
}
