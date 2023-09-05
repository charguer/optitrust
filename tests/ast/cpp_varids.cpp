class CC {
public:
    int * i;
    void f(int * a, int b) {
        i = a;
        *i = 1;
    }
    int q(int a);
};

int q2(int a) {
  CC cc;
  return cc.q(a);
}

int CC::q(int a) {
    return a;
}

class CC2 {
public:
    int * i;
    int * j;

    void f(int * a, int * c, int b) {
        //i = a;
        //j = c;
        //i = c;
        //*i = 1;
        //*j = 42;
        int h = this->q(1);
        //p(i, j);
    }

    int q(int a) {
      return a;
    }

    void p(int * a, int * b) { *a = *b + 1; }
};

class CC3 {
public:
    int * i;
    int * j;

    static void qf() {};

    void f(int * a, int * c, int b) {
        i = a;
        j = c;
        this->p(i, j);
        CC3::qf();
        qf();
        q(1);
    }

/* FIXME: "only fields are allowed in record declaration"
    class CC4 {
      public:
      void f(int) {
        return;
      }

      void g() {
        return f(0);
      }
    };
    CC4 cc4;
*/
    int q(int a) {
      // cc4.f(a);
      return a;
    }

    void p(int * a, int * b) { *a = *b + 1; }
};

void cc3() {
  CC3 cc3;
  cc3.q(0);
  // cc3.cc4.f(1);
}

void cc3_bis(CC3* cc3) {
  /* CC3* cc3 = new CC3; */
  cc3->q(0);
  /* delete cc3; */
}

namespace Q {
  void f();
/* LATER: namespace extension }

namespace Q { */
  void f() {
    Q::f();
    return;
  }
}