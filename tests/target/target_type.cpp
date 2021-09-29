int f(int n, double x) {
  double r = 0.0;
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 12; j++) {
       i = i + 1;
       j += 2;
       r = x;
    }
  }
  return 3 + (int) x;
}

bool g(bool b, double y) {
  bool c = !b;
  return c;
}

int main() {
  for (int i = 0; i < 3; i++) {
   f(i, 3.0);
  }
}
