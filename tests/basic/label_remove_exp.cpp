int main() {
  int x = 3;
  for (int i = 0; (i < 3); i++) {
    if (true) {
      x++;
    } else {
      i++;
    }
  }

stop:
  return 0;
}