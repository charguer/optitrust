class CC {
 public:
  int* i;
  void f__0(int* a__0, int b__0) {
    i = a__0;
    *i = 1;
  }
  int q(int a__1);
};

int q2(int a__2) {
  CC cc;
  return cc.q(a__2);
}

int CC::q(int a__3) { return a__3; }

class CC2 {
 public:
  int* i;
  int* j;
  void f__1(int* a__4, int* c__0, int b__1) { int h = this->q__0(1); }
  int q__0(int a__5) { return a__5; }
  void p(int* a__6, int* b__2) { *a__6 = *b__2 + 1; }
};

class CC3 {
 public:
  int* i;
  int* j;
  static void qf() {}
  void f__2(int* a__7, int* c__1, int b__3) {
    i = a__7;
    j = c__1;
    this->p__0(i, j);
    CC3::qf();
    qf();
    q__1(1);
  }
  int q__1(int a__8) { return a__8; }
  void p__0(int* a__9, int* b__4) { *a__9 = *b__4 + 1; }
};

void cc3__0() {
  CC3 cc3;
  cc3.q__1(0);
}

void cc3_bis(CC3* cc3__1) { cc3__1->q__1(0); }

namespace Q {
void f__3();
void f__3() {
  Q::f__3();
  return;
}
}  // namespace Q
