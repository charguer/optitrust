void printReference(int &x) {
  int y;
  y = x;
}

void printReference1(int &&x) {
  int y;
  y = (x + 1);
}

int main() {
  int const a = 2;
  int const b = (2 + 2);
  int const d = (2 + 2);
  int x = 3;
  int y = (3 + 3);
  int &u = y;
  u = 5;
  return 4;
}