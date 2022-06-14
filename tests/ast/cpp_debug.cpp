#include <vector>
#include <algorithm>

// foo::bar is a "qualified variable",
// same as modules in ocaml M.N.x
// type qvar =  { qvar_var = "x"; qvar_path = ["M";"N"]; qvar_str = "M.N.x" in OCaml or "M::N::x" in C }

void test_vector() {
  std::vector<int> v;   // vector<int> is a typ_constr (typ_constrid, typ_int)
  v.push_back(3);  // encoded as push_back(v,3)
  // int a = v[0];
}
int main(){}