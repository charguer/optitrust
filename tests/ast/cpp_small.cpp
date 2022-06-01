
int main() {}

class test_static_class {

private:

  static int foo(int x) {
     return x;
  }

public:

  static int bar(int x) {
     return x;
  }

};


class test_class {

private:

  int x;

public:

  void move(int d) {
     x += d;
  }

  bool test_this() {
    return this.x == x;
  }

};

template<typename T>
bool test_poly(T* x, T* y) {
  return x == y;
}


#include <vector>
#include <algorithm>

void test_vector() {
  std::vector<int> v;
  v.push_back(3);
  int a = v[0];
}

int test_iterator(std::vector<int> v) {
  int r = 0;
  for (auto it = std::begin(v); it != std::end(v); it++) {
      r += *it;
  }
  return r;
}

int test_lambda(std::vector<int> v) {
  int r = 0;
  std::for_each(std::begin(v), std::end(v), [&](int const& x) {
     r += x; });
  return r;
}
