#include <optitrust.h>

const int N = 2;
const int L = 4;
const int U = 7;

void f() {
  int s = 0;

  for (int i = s; i < s + N; i++) {
    int a = 1;
    int b = a + 2;
    int c = 3;
    int d = c + 4;
    int e = d + 5;
  }

  for (int j = L; j < U; j++){
    int x = j;
    int y = j + 1;
  }

  for (int k = 0; k < 2; k++) {
    for (int k2 = 0; k2 < 2; k2++) {
      s = k + k2;
    }
  }
}

void iter_contract_ro(int* M) {
  __reads("M ~> Matrix1(3)");

  int acc = 0;
  for (int x = 0; x < 3; x++) {
    __sequentially_modifies("&acc ~> Cell");
    __reads("&M[MINDEX1(3,x)] ~> Cell");
    acc += M[MINDEX1(3,x)];
  }
}
