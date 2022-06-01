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

