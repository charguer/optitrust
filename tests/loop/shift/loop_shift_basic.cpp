#include <optitrust.h>

int main(){
  __pure();

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 0; i < 10; i++){
    __sequentially_modifies("&x ~> Cell");
    x += i;
  }

  for (int j = st; j < st+N; j++){
    __sequentially_modifies("&x ~> Cell");
    x += j;
  }

  int shift = 5;
  for (int k = 0; k < N; k++){
    __sequentially_modifies("&x ~> Cell");
    x += k;
  }
/* FIXME:
  for (int l = N; l > 0; l--){
    __sequentially_modifies("&x ~> Cell");
    x += l;
  }
*/
  for (int m = 2; m < N-2; m++) {
    __sequentially_modifies("&x ~> Cell");
    x += m;
  }
}

void excl_array(int* t, int n) {
  __modifies("t ~> Matrix1(n)");

  for (int i = 0; i < n; i++){
    __modifies("&t[MINDEX1(n, i)] ~> Cell");
    t[MINDEX1(n, i)] += i;
  }
}
