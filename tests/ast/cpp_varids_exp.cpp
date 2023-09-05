class CC {
 public:
  int* i;
  void f(int* a, int b) {
    i = a;
    *i = 1;
  }
  int q(int a__0);
};

int q2(int a__1) {
  CC cc;
  return cc.q(a__1);
}

int CC::q(int a__2) { return a__2; }

class CC2 {
 public:
  int* i;
  int* j;
  void f__0(int* a__3, int* c, int b__0) { int h = this->q__0(1); }
  int q__0(int a__4) { return a__4; }
  void p(int* a__5, int* b__1) { *a__5 = *b__1 + 1; }
};

class CC3 {
 public:
  int* i;
  int* j;
  static void qf() {}
  void f__1(int* a__6, int* c__0, int b__2) {
    i = a__6;
    j = c__0;
    this->p__0(i, j);
    CC3::qf();
    qf();
    q__1(1);
  }
  int q__1(int a__7) { return a__7; }
  void p__0(int* a__8, int* b__3) { *a__8 = *b__3 + 1; }
};

void cc3__0() {
  CC3 cc3;
  cc3.q__1(0);
}

void cc3_bis(CC3* cc3__1) { cc3__1->q__1(0); }

namespace Q {
void f__2();
void f__2() {
  Q::f__2();
  return;
}
}  // namespace Q
