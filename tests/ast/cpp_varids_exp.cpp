class CC {
 public:
  int* i;
  void f(int* a, int b) {
    this->i = a;
    *this->i = 1;
  }
  int q(int a);
};

int q2(int a) {
  CC cc;
  return cc.q(a);
}

int CC::q(int a) { return a; }

class CC2 {
 public:
  int* i;
  int* j;
  void f(int* a, int* c, int b) { int h = this->q(1); }
  int q(int a) { return a; }
  void p(int* a, int* b) { *a = *b + 1; }
};

class CC3 {
 public:
  int* i;
  int* j;
  static void qf() {}
  void f(int* a, int* c, int b) {
    this->i = a;
    this->j = c;
    this->p(this->i, this->j);
    CC3::qf();
    qf();
    this->q(1);
  }
  int q(int a) { return a; }
  void p(int* a, int* b) { *a = *b + 1; }
};

void cc3() {
  CC3 cc3;
  cc3.q(0);
}

void cc3_bis(CC3* cc3) { cc3->q(0); }

namespace Q {
void f();
void f() {
  Q::f();
  return;
}
}  // namespace Q
