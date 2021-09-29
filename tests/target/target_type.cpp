int f(int n, double x) {
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 12; j++) {
       i += 1;
       j += 2;
    }
  }
  return 3 + (int) x;
}

bool g(bool b) {
  bool c = !b;
  return c;
}

int main() {
  for (int i = 0; i < 3; i++) {
   f(i, 3.0);
  }
}
