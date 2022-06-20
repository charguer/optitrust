
#include <vector>
#include <random>

int main(){
    const int Size = 10000000;
    std::vector<int> data(Size);

    std::mt19937 gen(0);
    std::uniform_int_distribution<> dis(1, Size);


    return 0;
}
