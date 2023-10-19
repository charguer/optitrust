int main() {
  int x = 0;
  for (int i2 = 0; i2 < 10; i2++) {
    x += i2 + 2;
  }
  int shift = 2;
  for (int k = shift; k < 10 + shift; k++) {
    x += k - shift;
  }
}
