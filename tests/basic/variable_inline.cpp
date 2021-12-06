void printReference (int& x) {
  int y;
  y = x;
}

void printReference1 (int&& x) {
  int y;
  y = x+1;
}

int main() {
  // inlining without removal of 'a'
  const int a = 2;
  const int b = a + a;

  // inlining with removal of 'c'
  const int c = 2;
  const int d = c + c;

  return z;
}

