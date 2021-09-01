int main() {
  int x = 0;
  for (int i = 0; i < 10; i++) {
    x += 1;
    if (true)
      x += 2;
    else {
      if (false) {
        x += 3;
      } else {
        x += 4;
      }
    }
    x += 5;
  }
  return 0;
}