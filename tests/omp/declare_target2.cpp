struct typeX {
  int a;
};

class typeY {
  int a;
  public:
    int foo(){return a ^ 0x01;}
};

struct typeX varX;

class typeY varY;


void foo(){
  varX.a = 100;
  varY.foo();

}
