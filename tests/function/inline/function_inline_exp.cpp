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
  int y = /*@bodyf*/ ({
    int a = x + x;
    const int __res = a + a;
    __res;
  }) /*bodyf@*/;
  int z = ({
    const int __res = x > 0 ? 1 : 2;
    __res;
  });
  int u = /*@bodyh*/ ({
    int res;
    if (x > 0) /*no-brace*/ {
      res = 1;
      goto exit;
    }
    res = 2;
  exit:;
    const int __res = res;
    __res;
  }) /*bodyh@*/;
  int* q;
  { (*q)++; }
  int result;
  result = 10;
  result = /*@bodyk*/ { const int __res = result + /*@substk*/ 4 /*substk@*/;
  __res;
} /*bodyk@*/;
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
