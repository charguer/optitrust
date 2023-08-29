#include "cpp_ast.h"

vect operator+(const vect& a, const vect& b)
{
    vect r =  { a.x + b.x, a.y + b.y };
    return r;
}

int main() {
  vect a = {0,0};
  vect b = {0,0};
  vect c = a + b;
}
// TODO: add support for return (vect) { a.x + b.x, a.y + b.y };


template<class A>
class queue {
  A item;
};

class test {
  static void Partition (int* outPivot, queue<int> data)
  {
    return;
  }

};

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