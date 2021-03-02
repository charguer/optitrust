int *t;

int main() {
  int X = 10;
  for (int bx = 0; (bx < X); bx = (bx + 2)) {
    for (int x = bx; (x < min(X, (bx + 2))); x++) {
      t[x] = 0;
    }
  }
  return 0;
}
