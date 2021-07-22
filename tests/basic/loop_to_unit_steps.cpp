#include <stdio.h>

int main(){
  int s = 0;
  int a = 6;
  int b = 11;
  const int B = 2;
  for (int i = a; i < b; i += B) {
    s += i;
  }
  printf("%d\n", s);
}