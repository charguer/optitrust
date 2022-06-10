
int main() {}

#include <vector>
#include <algorithm>

// foo::bar is a "qualified variable",
// same as modules in ocaml M.N.x
// type qvar =  { qvar_var = "x"; qvar_path = ["M";"N"]; qvar_str = "M.N.x" in OCaml or "M::N::x" in C }


void test_vector1() {
  // M :: N :: x;
  // int y = M :: N :: x;
  
  std::vector<int> v;   // vector<int> is a typ_constr (typ_constrid, typ_int)
  v.push_back(3);  // encoded as push_back(v,3)
  // int a = v[0];
}

using namespace std;

void test_vector2() {
  vector<int> v;   // vector<int> is a typ_constr (typ_constrid, typ_int)
  v.push_back(3);  // encoded as push_back(v,3)
  // int a = v[0];
}

// int test_iterator(std::vector<int> v) {
//   int r = 0;
//   for (auto it = std::begin(v); it != std::end(v); it++) { // auto type needs to be supported
//       r += *it;
//   }
//   return r;
// }

// void test_lambda(std::vector<int> v) {
//   int r = 0;
//   auto f = [&](int const& x) -> void { // we only support arguments by references [&]
//      r += x; }; // trm_fun
//   std::for_each(std::begin(v), std::end(v), f);
// }

// int test_lambda_inline(std::vector<int> v) {
//   int r = 0;
//   std::for_each(std::begin(v), std::end(v), [&](int const& x) { // we only support arguments by references [&]
//      r += x; }); // trm_fun
//   return r;
// }

// using namespace std;

// void test_using() {
//   vector<int> v;
//   v.push_back(3);
// }


