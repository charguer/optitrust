#include <optitrust.h>

void seq_array(){
  __pure();

  int x = 0;
  const int st = 0;
  const int N = 10;

  for (int i = 0; i < 10; i++){
    __smodifies("&x ~> Cell");
    x += i;
  }

  for (int j = st; j < st+N; j++){
    __smodifies("&x ~> Cell");
    x += j;
  }

  int shift = 5;
  for (int k = 0; k < N; k++){
    __smodifies("&x ~> Cell");
    x += k;
  }
/* FIXME:
  for (int l = N; l > 0; l--){
    __smodifies("&x ~> Cell");
    x += l;
  }
*/
  for (int m = 2; m < N-2; m++) {
    __strict();
    __smodifies("&x ~> Cell");
    x += m;
  }
}

void excl_array(int* t, int n) {
  __modifies("t ~> Matrix1(n)");

  for (int i = 0; i < n; i++){
    __xmodifies("&t[MINDEX1(n, i)] ~> Cell");
    t[MINDEX1(n, i)] += i;
  }
}

void non_transparent_ghosts(int* t, int n) {
  __writes("t ~> Matrix1(n)");

  __ghost(group_intro_zero, "items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  for (int i = 0; i < n; i++) {
    __smodifies("for i in 0..i -> &t[MINDEX1(n, i)] ~> Cell");
    __smodifies("for i in i..n -> &t[MINDEX1(n, i)] ~> UninitCell");

    __ghost(assume, "is_subrange(i..(i + 1), i..n)");
    __ghost(group_split, "split := i + 1, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
    for (int j = i; j < i + 1; j++) {
      __xwrites("&t[MINDEX1(n, j)] ~> Cell");
      t[MINDEX1(n, j)] = j;
    }
    __ghost(group_join, "split := i, items := fun i -> &t[MINDEX1(n, i)] ~> Cell");
  }
  __ghost(group_shift, "start := n, stop := n, shift := -n, new_start := 0, new_stop := 0, items := fun i -> &t[MINDEX1(n, i)] ~> UninitCell");
  __ghost(group_elim_zero, "items := fun i -> &t[MINDEX1(n, i - (-n))] ~> UninitCell");
}
