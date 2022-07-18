struct typeX {
  int a;
};

class typeY {
  int a;

 public:
  int foo() { return a ^ 1; }
};

#pragma omp declare target
typeX varX;

typeY varY;

#pragma omp end declare target
void foo() {
#pragma omp target
  {
    varX.a = 100;
    varY.foo();
  }
}
