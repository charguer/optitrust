#include <stdbool.h>

int main() {
  int x;
  x = 3;
  for (int i = 0; i < 3; i++) {
    if (true) {
      x++;
    } else {
      x--;
    }
  }
stop:
  return 0;
}
