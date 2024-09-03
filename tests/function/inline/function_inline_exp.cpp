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
    if (x > 0) /*no-brace*/ {
      u = 1;
    goto exit_body;
    }
    u = 2;
  } /*bodyh@*/
exit_body:;
  int* q;
  (*q)++;

  int result;
  result = 10;
  int;
  /*@bodyk*/ { result + /*@substk*/ 4 /*substk@*/; } /*bodyk@*/
}

class Test_method_inline {
 private:
  int u;

 public:
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
  int y = c.f(x);
  int y1 = c.f1();
  int z = c.g(x);
  int u = c.h(x);
  int* q;
  c.m(q);
  int result;
  result = 10;
  result = c.k(result, 4);
}

void test_nameclash() {
  int x = f(1);
  int y = f(2);
}

int recurse3() { return 0; }

int recurse2() {
  int a = 0;
  int b = 0;
  return a + b;
}

int recurse1() {
  int a = 0;
  int b = 0;
  int a1 = a + b;
  int b2 = 0;
  return a1 + b2;
}
