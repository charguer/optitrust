int f(int n, double x) {
  double r = 0.0;
  int i; /* Note: if i is declared inside the loop, we are not allowed to mutate the index in the loop ;
          TODO: the loop for (i = 0; i < 10; i++)  should not be treated as a simple loop */
  for (i = 0; i < 10; ) {
    for (int j = 0; j < 12; j++) {
       i = i + 1;
       j += 2;
       r = x;
    }
    i++; /* TODO: put this back into the loop after bug above is fixed */
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
