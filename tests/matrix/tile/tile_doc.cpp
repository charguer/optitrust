#include <optitrust.h>
int main() {
  float *a = MALLOC1(float, 10);
  for (int i = 0; i < 10; i++) {
    a[MINDEX1(10,i)] = i +1;
  }
}
