#include <optitrust_models.h>

int main() {
  int x = 0;
  int y = x;
sec: {
  y = y + 1;
  y = y + 2;
}
  x = y;
  int r = x;
}
