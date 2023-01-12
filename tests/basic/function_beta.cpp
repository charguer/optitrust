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

int main (){}
