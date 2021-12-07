int main() {
  int const x = 3;
  int const y = (x + x);
  const int &u = y;
  int const z = 4;
  return z;
}
