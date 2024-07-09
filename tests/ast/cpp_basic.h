
#include <iostream>

class AClassWithMethods{
public:
    int methodWithConst() const{
        std::cout << "methodWithConst" << std::endl;
        return 0;
    }
    
    int methodWithoutConst() {
        std::cout << "methodWithoutConst" << std::endl;
        return 0;    
    }
    
    int uniqueNameConstNoConst() const {
        std::cout << "uniqueNameConstNoConst const" << std::endl;
        return 0;    
    }
    
    int uniqueNameConstNoConst() {
        std::cout << "uniqueNameConstNoConst no const" << std::endl;
        return 0;    
    }
};


void functionOverloading(){
    std::cout << "void" << std::endl;
}

void functionOverloading(int){
    std::cout << "int" << std::endl;
}

void functionOverloading(double){
    std::cout << "double" << std::endl;
}

void functionOverloading(char*, int* ptr = nullptr){
    std::cout << "char*, int* ptr = nullptr" << std::endl;
}


class AClassWithOverMethods{
public:
    void methodOverloading() const{
        std::cout << "void const" << std::endl;
    }
    
    void methodOverloading(int) const{
        std::cout << "int const" << std::endl;
    }
    
    void methodOverloading(double) const{
        std::cout << "double const" << std::endl;
    }
    
    void methodOverloading(char*, int* ptr = nullptr) const{
        std::cout << "char*, int* ptr = nullptr const" << std::endl;
    }
    
    void methodOverloading(){
        std::cout << "void" << std::endl;
    }
    
    void methodOverloading(int){
        std::cout << "int" << std::endl;
    }
    
    void methodOverloading(double){
        std::cout << "double" << std::endl;
    }
    
    void methodOverloading(char*, int* ptr = nullptr){
        std::cout << "char*, int* ptr = nullptr" << std::endl;
    }
};


class AClassWithTempMethods{
public:
    template <class AType>
    int templateMethod() const{
        std::cout << "templateMethod" << std::endl;
        return 0;
    }
    
    template <class AType>
    int templateMethod(AType aValue) const{
        std::cout << "templateMethod with a value" << std::endl;
        return 0;
    }
};

