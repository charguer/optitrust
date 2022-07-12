int f(int x) { return x + 1; }

int g(int x, int y) { return x + y; }

int h() { return 1; }

void test_function() {
  int a = 3;
  const int r = g(a, 4);
  int t = f(r);
  const int b = f(a);
  int u = f(b);
  const int s = h();
  int z = f(s);
  return 0;
}

class Test_method {
  int x;

 public:
  void f(int y) { x = x + y; }
  int g(int y) { return x + y; }
};

void test_method() {
  int a = 3;
  Test_method t;
  const auto s = t.g(a);
  a = s;
  const auto r = t.g(a);
  int b = r;
}

int main() {}
