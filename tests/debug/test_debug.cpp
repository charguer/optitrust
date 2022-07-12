class X {

  int x;
  public:
    int f_X(int y){
      return x + y;
    }
};

void test_method(){
  int i = 1;
  X a;
  i = a.f_X(i);

}

int main () {}