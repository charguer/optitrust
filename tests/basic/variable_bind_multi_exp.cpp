int main() {
  int s = 0;
  for (int i = 0; i < 3; i++) {
    s = 2 + 3;
  }
  int t = 0;
  int b = 2 + s;
  for (int j = 0; j < 3; j++) {
    /*no-brace*/ { t += b; }
    t += b;
  }
  int r = (4 + 3) * (4 + 3) + (4 + 4) * (4 + 4);
  return 0;
}
