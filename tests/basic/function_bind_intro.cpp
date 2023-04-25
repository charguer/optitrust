int f(int x) {
  return x+1;
}

int g(int x, int y) {
  return x + y;
}

int h() {
  return 1;
}

void test_function (){
  int a = 3;
  int t = f(g(a, 4));
  int u = f(f(a));
  int z = f(h());
}

class Test_method {
  int x;

  public:
    void f(int y){
      x = x + y;
    }
    int g(int y){
      return x + y;
    }

};


void test_method(){
  int a = 3;
  Test_method t;
  a = t.g(a);
  int b = t.g(a);
}

int main() {}
