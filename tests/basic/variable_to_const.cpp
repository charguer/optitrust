int main() {

  int x = 3;
  int y = x + x;

  int &u = y; // it would not be valid to make y const, and keep a reference on it

  // u = 5;
  int z = 4;

  return z;
}

