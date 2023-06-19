#include <vector>

#include <algorithm>

class test_static_class {
 private:
  static int foo(int x) { return x; }

 public:
  static int bar(int x) { return x; }
};

class test_class {
 private:
  int x;

 public:
  void move_this(int d) { this->x += d; }
  void move(int d) { x += d; }
  bool test_this() { return this->x == x; }
};

template <typename T>
bool test_poly(T* x, T* y) {
  return x == y;
}

void test_vector() {
  std::vector<int> v;
  v.push_back(3);
  int a = v[0];
}

int test_iterator(std::vector<int> v) {
  int r = 0;
  for (auto it = std::begin(v); it != std::end(v); it++) {
    r += (*it);
  }
  return r;
}

void test_lambda(std::vector<int> v) {
  int r = 0;
  auto f = [&](const int& x) -> void { r += x; };
  std::for_each(std::begin(v), std::end(v), f);
}

int test_lambda_inline(std::vector<int> v) {
  int r = 0;
  std::for_each(std::begin(v), std::end(v), [&](const int& x) { r += x; });
  return r;
}

using namespace std;

void test_using() {
  vector<int> v;
  v.push_back(3);
  int a = v[0];
}

template <typename A, typename B>
struct Box {
 public:
  A key;
  B value;
};

template <typename A, typename B>
void update(Box<A, B>* b, A key, B value) {
  b->key = key;
  b->value = value;
}

class Box2 {
  Box<int, bool> b;

 public:
  void update1(int key, bool value) { update<int, bool>(&b, key, value); }
};

template <typename A>
class Inject {
  static void f(Inject x) {
    Inject<A> y;
    Inject z;
  }
};

class Test {
  int x;

 public:
  Test();
  Test(int x);
  int get();
  void set(int y);
};

Test::Test(int y) {}

Test::Test() { x = 0; }

int Test::get() { return x; }

void Test::set(int y) { x = y; }

class A {
  int a;
  int b;

 public:
  A(int i) { a = i; }
  A(int i, int j) { a = i; }
  A(int i, int j, int k) { a = i; }
};

int main() {
  Box<int, bool> b;
  update<int, bool>(&b, 1, true);
  Box<float, bool> b1;
  update<float, bool>(&b1, 1., true);
  Box2 b2;
  b2.update1(1, true);
  Test t(10);
  t.set(10);
  int y = t.get();
  A x1(1);
  A y1(1, 2);
  A z1(1, 2, 3);
  return 0;
}
