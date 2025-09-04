#include <iostream>
#include <vector>
#include <functional>
int main() {
    std::vector<int> v = {1,2,3};
    for (int n : v) std::cout << n << "\n";
}
