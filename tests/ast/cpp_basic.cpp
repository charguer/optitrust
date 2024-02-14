#include "cpp_basic.h"

#include <vector>

int main(){
    {
        AClassWithMethods anObject;
        anObject.methodWithConst();
        anObject.methodWithoutConst();
        anObject.uniqueNameConstNoConst(); // uniqueNameConstNoConst NO const
    }
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
    {
        std::vector<int> vecOfInt;
    }
    {
        std::vector<int> vecOfInt(100);
    }
    {
        std::vector<int> vecOfInt = std::vector<int>(100);
    }
    return 0;
}

