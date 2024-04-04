#include <iostream>
#include <vector>

class AClassWithMethods {
 public:
  int methodWithConst() const {
    std::cout << "methodWithConst" << std::endl;
    return 0;
  }
  int methodWithoutConst() {
    std::cout << "methodWithoutConst" << std::endl;
    return 0;
  }
  int uniqueNameConstNoConst() {
    std::cout << "uniqueNameConstNoConst no const" << std::endl;
    return 0;
  }
};

int main() {
  {
    AClassWithMethods anObject;
    anObject.methodWithConst();
    anObject.methodWithoutConst();
    anObject.uniqueNameConstNoConst();
  }
  {
    int c = 0;
    std::vector<int> vecOfInt;
  }
  { std::vector<int> vecOfInt(100); }
  return 0;
}
