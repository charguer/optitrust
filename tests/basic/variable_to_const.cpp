void printReference (int& x) {
  int y;
  y = x;
}

void printReference1 (int&& x) {
  int y;
  y = x+1;
}

int main() {

  // inlinine of not a constant
  int x = 3;
  int y = x + x;

  // inlining in a return expression
  int &u = y;
  u = 5;
  int z = 4;

  return z;
}

