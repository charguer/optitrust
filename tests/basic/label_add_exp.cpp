int main() {
start:
  int x = 3;
  for (int i = 0; i < 3; i++) {
  cond:
    if (1) {
    incr_1:
      x++;
    } else {
    incr_2:
      x--;
    }
  }
  x++;
stop:
  return 0;
}
