#include <stdio.h>

int main(){
  int N = 10;
  int C = 2;
  int D = 2;
  int total1 = 0;
  int total2 = 0;
  for (int i = 0; i < N; i++) {
    total1 += i;
    
  }
  printf("%d\n", total1);
  for (int j = 0; j < N; j += 2) {
    total2 += j;
  }
  printf("%d\n", total2);

  return 0;
}