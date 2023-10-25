void f(int j) {
  int s = 0;
  s += 2 * j;
  s -= j;
}

void test_fun() {
  int i = 1;
  int s = 0;
  s += 2 * i;
  s -= i;
}

class X {
  int x;

 public:
  int f_X(int y) { return x + y; }
};

void test_method() {
  int a = 1;
  X b;
  int c;
  c = b.x + a;
}

namespace Y {
void h(int& a) { a = 1; }
}  // namespace Y

namespace Z {
void g(int a, int, int) { Y::h(a); }
}  // namespace Z

int main() {
  int x = 1;
  Y::h(x);
}
