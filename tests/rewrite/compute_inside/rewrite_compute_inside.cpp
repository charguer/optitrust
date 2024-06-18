#include <stdbool.h>
int main() {

  block1:{
    int x;
    x = 6 + 2;
    x = 6 * 2;
    x = 9 / 2;
    x = 10 % 6;
    x = 2 + 2 + 2 + 2;
    x = 2 + 2 - 2 + 2;
  }
  block2: {
  float y;
  y = 1.0 + 2.5;
  y = 2.5 * 2.5;
  y = 9.0 / 2.1;
  y = 2.5 + 2.5 + 2.5 + 2.5;
  y = 2.5 + 2.5 - 2.5 + 2.5;
  }
  block3: {
  bool a;
  bool b;
  bool c;
  c = true || a;
  c = false || a;
  c = a || true;
  c = a || false;
  c = true && a;
  c = false && a;
  c = a && true;
  c = a && false;
  c = a || (b && true) || true;
  c = !true;
  c = !false;
  a = a && (b && true || false);
  }
  return 0;
}
