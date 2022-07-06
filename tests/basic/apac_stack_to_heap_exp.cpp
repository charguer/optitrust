int main() {
  int i;
  int* __APAC_a = new int(1);
  int& a = *__APAC_a;
  int* __APAC_b = new int(i);
  int& b = *__APAC_b;
  const int* __APAC_c = new const int(1);
  const int& c = *__APAC_c;
  const int* __APAC_d = new const int(i);
  const int& d = *__APAC_d;
}