#include <optitrust.h>

void f() {
  __pure();
  int x = 0;
  const int st = 0;
  const int N = 10;
  __ghost(to_prove, "P := is_subrange(0..1, 0..10)");
  __ghost(assume, "P := in_range(0, 0..1)");
  __ghost(assume, "P := in_range(0, 0..10)");
  x += 0;
  for (int i = 1; i < 10; i++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(i, 0..10)");
    x += i;
  }
  __ghost(to_prove, "P := is_subrange(st..(st + 1), st..N)");
  __ghost(assume, "P := in_range(st, st..(st + 1))");
  __ghost(assume, "P := in_range(st, st..N)");
  x += st;
  for (int j = st + 1; j < N; j++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(j, st..N)");
    x += j;
  }
  int cut = 5;
  __ghost(to_prove, "P := is_subrange(0..1, 0..N)");
  __ghost(assume, "P := in_range(0, 0..1)");
  __ghost(assume, "P := in_range(0, 0..N)");
  x += 0;
  for (int k = 1; k < N; k++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(k, 0..N)");
    x += k;
  }
  __ghost(to_prove, "P := is_subrange(st..(st + 1), st..N)");
  __ghost(assume, "P := in_range(st, st..(st + 1))");
  __ghost(assume, "P := in_range(st, st..N)");
  x += st;
  for (int l = st + 1; l < N; l++) {
    __strict();
    __smodifies("&x ~> Cell");
    __ghost(assume, "P := in_range(l, st..N)");
    x += l;
  }
}
