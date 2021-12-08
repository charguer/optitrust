#include <stdio.h>

int main() {
  int s = 0;
  int a = 6;
  int b = 10;
  const int B = 2;
  for (int j = 0; (j < ((b - a) / B)); j++) {
    int i = (a + (j * B));
    s += i;
  }
  printf("%d\n", s);
}
