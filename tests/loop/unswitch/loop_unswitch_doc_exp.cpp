#include <stdbool.h>

void f(bool b) {
  int s;
  int t;
  if (b)
    for (int i = 0; i < 3; i++) {
      s += i;
    }
  else
    for (int i = 0; i < 3; i++) {
      t += i;
    }
}
