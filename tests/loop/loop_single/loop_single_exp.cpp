#include <optitrust.h>

int main() {
  const int i = 24;
  for (int k = i + 3; k < i + 3 + 1; k++) {
    int j = 100 * 3 + i;
    int l = j + i;
  }
}

int main2() {
  const int i = 24;
  int res;
  for (int k = 3; k < 3 + 1; k++) {
    int j = 100 * 3 + i;
    const int __res = 10;
    res = __res;
  }
  return res;
}

int main3() {
  const int i = 24;
  for (int k = i + 3; k < i + 3 + 1; k++) {
    int j = 100 * 3 + i + k;
  }
}
