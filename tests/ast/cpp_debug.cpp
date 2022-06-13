#include <vector>
#include <algorithm>



int test_iterator(std::vector<int> v) {
  int r = 0;
  for (auto it = std::begin(v); it != std::end(v); it++) { // auto type needs to be supported
      r += *it;
  }
  return r;
}

void test_lambda(std::vector<int> v) {
  int r = 0;
  auto f = [&](int const& x) -> void { // we only support arguments by references [&]
     r += x; }; // trm_fun
  std::for_each(std::begin(v), std::end(v), f);
}

int test_lambda_inline(std::vector<int> v) {
  int r = 0;
  std::for_each(std::begin(v), std::end(v), [&](int const& x) { // we only support arguments by references [&]
     r += x; }); // trm_fun
  return r;
}


// using namespace std;

// void test_using() {
//   vector<int> v;
//   v.push_back(3);
// }


/*

template<typename A, typename B>
typedef struct { A key; B value; } box;

template<typename A, typename B>
void update(box<A,B>* b, A key, B value) {
  b->key = key;
  b->value = value;
}

int main() {
  box<int,bool> b;
  update(&b,1,true);
  update<int,bool>(&b,1,true);
}


---

class box2 {
  box<int,bool> b;
}

b.update(1,true)



encodings of method calls:
  x.f(y)    function is an access whose LHS is a object (i.e. a value whose type is of some class)
-> 
  f(x,y) @ method_call
*/