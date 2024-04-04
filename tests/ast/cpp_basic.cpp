#include <iostream>
#include <vector>

// TODO: Function and method overloading.
// TODO: Template methods.

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
  /*  
  int uniqueNameConstNoConst() const {
    std::cout << "uniqueNameConstNoConst const" << std::endl;
    return 0;    
  } 
  */  
  int uniqueNameConstNoConst() {
    std::cout << "uniqueNameConstNoConst no const" << std::endl;
    return 0;    
  }
};
/*
void functionOverloading() {
  std::cout << "void" << std::endl;
}

void functionOverloading(int) {
  std::cout << "int" << std::endl;
}

void functionOverloading(double) {
  std::cout << "double" << std::endl;
}

void functionOverloading(char*, int* ptr = nullptr) {
  std::cout << "char*, int* ptr = nullptr" << std::endl;
}

class AClassWithOverMethods{
public:
  void methodOverloading() const {
    std::cout << "void const" << std::endl;
  }
    
  void methodOverloading(int) const {
    std::cout << "int const" << std::endl;
  }
    
  void methodOverloading(double) const {
    std::cout << "double const" << std::endl;
  }
    
  void methodOverloading(char*, int* ptr = nullptr) const {
    std::cout << "char*, int* ptr = nullptr const" << std::endl;
  }
    
  void methodOverloading() {
    std::cout << "void" << std::endl;
  }
    
  void methodOverloading(int) {
    std::cout << "int" << std::endl;
  }
    
  void methodOverloading(double) {
    std::cout << "double" << std::endl;
  }
    
  void methodOverloading(char*, int* ptr = nullptr) {
    std::cout << "char*, int* ptr = nullptr" << std::endl;
  }
};

class AClassWithTempMethods {
public:
  template <class AType>
  int templateMethod() const {
    std::cout << "templateMethod" << std::endl;
    return 0;
  }
    
  template <class AType>
  int templateMethod(AType aValue) const {
    std::cout << "templateMethod with a value" << std::endl;
    return 0;
  }
}; 
*/
int main(){
  {
    AClassWithMethods anObject;
    anObject.methodWithConst();
    anObject.methodWithoutConst();
    anObject.uniqueNameConstNoConst(); // uniqueNameConstNoConst NO const
  }
  /*
  {
    const AClassWithMethods anObject;
    anObject.methodWithConst();
    anObject.uniqueNameConstNoConst(); // uniqueNameConstNoConst const
  }
  {
    functionOverloading();
    functionOverloading(1);
    functionOverloading(1.0);
    functionOverloading(nullptr);
    functionOverloading(nullptr, nullptr);
  }
  {
    AClassWithOverMethods anObject;
    anObject.methodOverloading();
    anObject.methodOverloading(1);
    anObject.methodOverloading(1.0);
    anObject.methodOverloading(nullptr);
    anObject.methodOverloading(nullptr, nullptr);
  }
  {
    const AClassWithOverMethods anObject;
    anObject.methodOverloading();
    anObject.methodOverloading(1);
    anObject.methodOverloading(1.0);
    anObject.methodOverloading(nullptr);
    anObject.methodOverloading(nullptr, nullptr);
  }
  {
    AClassWithTempMethods anObject;
    anObject.templateMethod<int>();
    anObject.templateMethod<float>();
    anObject.templateMethod<int>(0);
    anObject.templateMethod<float>(0);
  }
  */
  {
    std::vector<int> vecOfInt;
  }
  {
    std::vector<int> vecOfInt(100);
  }
  /*
  {
    std::vector<int> vecOfInt = std::vector<int>(100);
  }
  */
  return 0;
}

