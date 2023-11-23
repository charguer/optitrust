#include <optitrust.h>

int * t;
int main(){
  int a = 5;
  int b = 6;
  for (int i = 0; i < 10; i++) {
    int r = i;
    for (int j = 0; j < 10; j++) {
      int s = i;
      int x = a + b;
      t[i] = i;
    }
  }
  return 0;
}
