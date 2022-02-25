#include <stdbool.h>

int main() {
block1 : {
  int x;
  x = 8;
  x = 12;
  x = 4;
  x = 4;
  x = 8;
  x = 4;
}
block2 : {
  float y;
  y = 3.5;
  y = 6.25;
  y = 4.28571428571;
  y = 10.;
  y = 5.;
}
block3 : {
  bool a;
  bool b;
  bool c;
  c = true;
  c = false;
  c = true;
  c = a;
  c = a;
  c = false;
  c = a;
  c = false;
  c = a || b || true;
  c = false;
  c = true;
  a = a & b;
}
  return 0;
}
