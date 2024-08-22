#include <stdlib.h>

void e(int* a) { free(a); }

int f(int a) {
  a++;
  int b = a;
  b = 1;
  return b;
}

void g(int a[2]) { a[f(1)]--; }

void h(int a) { a = 1; }

void j(const int& a) { h(a); }

void k(int* a, int* b, int c) {
  int* d = a;
  d = b;
  *d = 1;
}

void l1(int& a) { a += 1; }

void l2(int& a) { l1(a); }

void l3(int& a) {
  int& b = a;
  l2(b);
}

void l4(int& a) { --a; }

int l5(int a) { return a; }

void m(const int& a, int* b, const int* const& c) {
  int* d = a + b;
  d[0] = 1;
}

int* n1(int* a, const int& b) {
  int* c = a;
  return c;
}

int* n2(int* a, const int& b) {
  int* c = a;
  return n1(c, b);
}

int& n3(int& a, const int& b) { return a; }

int& n4(int& a, const int& b) {
  int& c = a;
  return n3(c, b);
}

void n5(int* a, int& b, int c) {
  int* d = n2(a, c);
  int& e = n4(b, c);
}

void o0(int a, int b, int c) {
  int &d = a, *e = &b;
  d = 1;
  *e = 1;
}

void o1(int*& a) {
  int*& d = a;
  d = NULL;
}

void o2(int* a) {
  int*& d = a;
  d = NULL;
}

void o3(int a) {
  int& d = a;
  d = 42;
}

void c1(int* a, const int* b, const int* c, int* d) {
  int* aa = a;
  const int* bb = b;
  const int* cc = c;
  int* dd = d;
  *aa = 42;
  *dd = 12;
}

void c2(int* a, const int* b, const int* c, int* d) {
  int *aa = a, *dd = d;
  const int *bb = b, *cc = c;
  *aa = 42;
  *dd = 12;
}

void c3(int* a, const int* b, const int* c, int* d) {
  int *aa = a, *dd = d;
  const int *bb = b, *cc = c, e = 2;
  *aa = 42;
  *dd = 12;
}

namespace BB {
void h(int& a) { a = 1; }
}  // namespace BB

namespace AA {
void f(int& a) { a = 1; }
void g(int a, int b, int c) {
  f(a);
  BB::h(b);
}
}  // namespace AA

void p(int a, int b) { AA::f(a); }

class CC {
 public:
  int* i;
  int* j;
  void f(int* a, int b) {
    i = a;
    *j = 42;
    p(i, j);
  }
  int q(int a) const;
  void p(int* a, const int* b) const { *a = *b + 1; }
};

int CC::q(int a) const { return a; }

void q(CC a, int b, int c) { a.f(&b, 1); }
