class CC {
 public:
  int* i;
  void f(int* a, int b) {
    i = a;
    *i = 1;
  }
  int q(int a_2);
};

int q2(int a_3) {
  CC cc;
  return cc.q(a_3);
}

int CC::q(int a_4) { return a_4; }

class CC2 {
 public:
  int* i;
  int* j;
  void f_2(int* a_5, int* c, int b_2) { int h = this->q_2(1); }
  int q_2(int a_6) { return a_6; }
  void p(int* a_7, int* b_3) { *a_7 = *b_3 + 1; }
};

class CC3 {
 public:
  int* i;
  int* j;
  static void qf() {}
  void f_3(int* a_8, int* c_2, int b_4) {
    i = a_8;
    j = c_2;
    this->p_2(i, j);
    CC3::qf();
    qf();
    q_3(1);
  }
  int q_3(int a_9) { return a_9; }
  void p_2(int* a_10, int* b_5) { *a_10 = *b_5 + 1; }
};

void cc3_2() {
  CC3 cc3;
  cc3.q_3(0);
}

void cc3_bis(CC3* cc3_3) { cc3_3->q_3(0); }

namespace Q {
void f_4();
void f_4() {
  Q::f_4();
  return;
}
}  // namespace Q

namespace Q2 {
void f_5() {}
}  // namespace Q2

void Q2::f_5();

namespace N2 {
int x;
void f_6() { int x_2; }
int g() { return x; }
}  // namespace N2

int qskj() { return N2::x; }
