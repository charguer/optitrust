#include <optitrust.h>

int main(){

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 0; i < 10; i++){
    x += i;
  }

  for (int j = st; j < st+N; j++){
    x += j;
  }

  int shift = 5;
  for (int k = 0; k < N; k++){
    x += k;
  }

  for (int l = N; l > 0; l--){
    x += l;
  }
}

void contract(int* t, int n) {
  __modifies("t ~> Matrix1(n)");

  for (int i = 0; i < n; i++){
    __xmodifies("&t[MINDEX1(n, i)] ~> Cell");

    t[MINDEX1(n, i)] += i;
  }
}
