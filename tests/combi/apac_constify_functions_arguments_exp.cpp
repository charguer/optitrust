int f(const int a) {
  int b = a;
  b = 1;
  return b;
}

void g(int a[2]) { a[f(1)]--; }

void h(int& a) { a = 1; }

void j(int& a) { h(a); }

void k(int* a, int* b, const int c) {
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

void n5(int* a, int& b, const int c) {
  int* d = n2(a, c);
  int& e = n4(b, c);
}

void o(int a, int b, const int c) {
  int &d = a, *e = &b;
  d = 1;
  *e = 1;
}
