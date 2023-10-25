#include <stdbool.h>

int main() {
  int x;
start:
  x = 3;
  for (int i = 0; i < 3; i++) {
  cond:
    if (true) {
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
