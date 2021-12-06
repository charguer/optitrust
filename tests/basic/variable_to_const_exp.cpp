void printReference(int &x) {
  int y;
  y = x;
}

void printReference1(int &&x) {
  int y;
  y = (x + 1);
}

int main() {
  int const x = 3;
  int const y = (x + x);
  int &u = y;
  u = 5;
  int const z = 4;
  return z;
}
