int main() {
start:
  int x = 3;
  for (int i = 0; (i < 3); i++) {
  cond:
    if (true) {
    incr_1:
      x++;
    } else {
    incr_2:
      i++;
    }
  }
  x++;
stop:
  return 0;
}
