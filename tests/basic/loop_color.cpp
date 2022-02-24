#include <stdio.h>

int main(){
  int N = 10;
  int C = 2;
  int D = 2;
  int s = 0;
  for (int i = 0; i < 10; i++) {
    s += i;
  }
  for (int j = 0; j < 10; j += 2) {
    s += j;
  }
  printf("%d\n", s);
  return 0;
}