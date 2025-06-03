#include <optitrust.h>
#include <stdlib.h>
int main() {
  const int N = 5000;
  for (int i = 0; i < N; i++) {
    if (i >= 10 && i < 15) {
      i++;
    }
  }
}
int main2() {
  const int N = 5000;
  for (int i = 0; i < N; i++) {
    if (10 <= i && i < 15) {
      i++;
    }
  }
}
int main3() {
  const int N = 5000;
  for (int i = 0; i < N; i++) {
    if (10 <= i && i < 15 && i < 18) {
      i++;
    }
  }
}
