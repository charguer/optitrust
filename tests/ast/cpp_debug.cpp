class A {
  int a;
  int b;
public:
  A(int i) { a = i; }
  A(int i, int j) { a = i; }
  A(int i, int j, int k) { a = i; }
};

int main() {
  A x(1); // works
  A y(1, 2); // error
  A z(1, 2, 3); // error
  return 0;
