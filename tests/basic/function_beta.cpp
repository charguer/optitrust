void f(int j) {
  int s = 0;
  s += 2*j;
  s -= j;

}

void test_fun(){
  int i = 1;
  f(i);

}

class X {

  int x;
  public:
    int f_X(int y){
      return x + y;
    }
};

void test_method(){
  int a = 1;
  X b;
  int c = b.f_X(a);

}

namespace Y {
  void h(int& a) {
    a = 1;
  }
}

namespace Z {
  void g(int a, int, int) {
    Y::h(a);
  }
}

int main () {
  int x = 1;
  Y::h(x);
}
