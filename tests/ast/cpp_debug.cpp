#include <vector>

void test_vector() {
  std::vector<int> v;   // vector<int> is a typ_constr (typ_constrid, typ_int)
  v.push_back(3);  // encoded as push_back(v,3)
  int a = v[0];
}

int main(){}