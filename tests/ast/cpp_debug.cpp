// #include <random>

// #include <vector>

template <typename A> 
class Foo {

  static void f(Foo x ) { 
    Foo<A> y;
    Foo z; // encoded as (Foo<A>@Annot_injected) y;
  }


};


// int main(){
//     const int Size = 10000000;
//     std::vector<int> data(Size);

//     std::mt19937 gen(0);
    

//     return 0;
// }
