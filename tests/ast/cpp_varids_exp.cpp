class CC {
 public:
  int* i;
  void f_2(int* a_2, int b_2) {
    i = a_2;
    *i = 1;
  }
  int q(int a_3);
};

int q2(int a_4) {
  CC cc;
  return cc.q(a_4);
}

int CC::q(int a_5) { return a_5; }

class CC2 {
 public:
  int* i;
  int* j;
  void f_3(int* a_6, int* c_2, int b_3) { int h = this->q_2(1); }
  int q_2(int a_7) { return a_7; }
  void p(int* a_8, int* b_4) { *a_8 = *b_4 + 1; }
};

class CC3 {
 public:
  int* i;
  int* j;
  static void qf() {}
  void f_4(int* a_9, int* c_3, int b_5) {
    i = a_9;
    j = c_3;
    this->p_2(i, j);
    CC3::qf();
    qf();
    q_3(1);
  }
  int q_3(int a_10) { return a_10; }
  void p_2(int* a_11, int* b_6) { *a_11 = *b_6 + 1; }
};

void cc3() {
  CC3 cc3_2;
  cc3_2.q_3(0);
}

void cc3_bis(CC3* cc3_3) { cc3_3->q_3(0); }

namespace Q {
void f_5();
void f_5() {
  Q::f_5();
  return;
}
}  // namespace Q
