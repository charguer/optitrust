int foo(int x) { return x + x; }

int bar() { 
  int y = foo(3);
  return y + y;
}



#include <iostream>

double f(double x,double y){
    return x+y;
}

typedef struct{
  double x, y, z;
}vect;

vect vect_add(vect v1, vect v2) {
  vect v = { f(v1.x,v2.x),
             f(v1.y,v2.y),
             f(v1.z,v2.z)};
  return v;
}

vect vect_mul(double d, vect v) {
  vect w = { d * v.x,
             d * v.y,
             d * v.z };
  return w;
}



vect r;
int main() {
    vect v1,v2;
    r = vect_add(v1,v2);
    return 0;
}