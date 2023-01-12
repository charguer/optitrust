int f(int x) {
  int a = x + x;
  return a + a;
}

int g(int x) {
  if (x > 0)
    return 1;
  else
    return 2;
}

int h(int x) {
  if (x > 0) return 1;
  return 2;
}

void m(int* p) { (*p)++; }

int k(int a, int b) { return a + b; }

void test_fun() {
  int x = 3;
  int y;
  /*@bodyf*/ {
    int a = x + x;
    y = a + a;
  } /*bodyf@*/

  int z;
  if (x > 0)
    z = 1;
  else
    z = 2;

  int u;
  /*@bodyh*/ {
    if (x > 0) u = 1;
    goto exit_body;
    u = 2;
  } /*bodyh@*/
exit_body:;
  int* q;
  (*q)++;

  int result;
  result = 10;
  int;
  /*@bodyk*/ { result + 4; } /*bodyk@*/
}

class Test_method_inline {
 public:
  int u;
  int f(int x) {
    int a = x + x;
    return a + a;
  }
  int f1() {
    int a = u + u;
    return a + a;
  }
  int g(int x) {
    if (x > 0)
      return 1;
    else
      return 2;
  }
  int h(int x) {
    if (x > 0) return 1;
    return 2;
  }
  void m(int* x) { (*x)++; }
  int k(int a, int b) { return a + b; }
};

void test_class_method() {
  Test_method_inline c;
  int x = 3;
  int y;
  /*@bodyf1*/ {
    int a = x + x;
    y = a + a;
  } /*bodyf1@*/

  int y1;
  /*@bodyf1*/ {
    int a = c.u + c.u;
    y1 = a + a;
  } /*bodyf1@*/

  int z;
  if (x > 0)
    z = 1;
  else
    z = 2;

  int u;
  /*@bodyh*/ {
    if (x > 0) u = 1;
    goto exit_body;
    u = 2;
  } /*bodyh@*/
exit_body:;
  int* q;
  (*q)++;

  int result;
  result = 10;
  int;
  /*@bodyk*/ { result + 4; } /*bodyk@*/
}

int main() {}
