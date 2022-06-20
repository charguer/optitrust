#include <vector>
#include <random>

class Box {

  int width, height;

  public:
    Box (int w, int h){
      this->width = w;
      this->height = h;
    }
};

int main(){

  const int Size = 10000000;
    std::vector<int> data(Size);

    std::mt19937 gen(0);
}
