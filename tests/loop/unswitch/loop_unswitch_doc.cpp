
#include <stdbool.h>
bool b;

int main() {
  int s;
  int t;
  for (int i = 0; (i < 3); i++) {
    if (b) {
      s += i;
    } else {
      t += i;
    }
  }
}
