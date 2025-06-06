#include <optitrust.h>

// Nominal case
int main() {

  const int i = 24;
  const int k = i + 3;
  int j = 100 * 3 + i;
  int l = j + i;
}
// Seq with res
int main2() {

  const int i = 24;
  const int k = 3;
  int j = 100 * 3 + i;
  return 10;
}
// k is used
int main3() {

  const int i = 24;
  const int k = i + 3;
  int j = 100 * 3 + i + k;
}
