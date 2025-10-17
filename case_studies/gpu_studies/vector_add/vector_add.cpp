#include <optitrust_models.h>

template<typename T> T __GPU_GET(T *p);

// TODO how to do it with floats? it won't parse "add_x(5.0, A)"
__DEF(add_x, "fun (x: int) (A: int -> int) -> fun (i : int) -> A(i) + x");

void vector_add(int *a, int *b, int N) {
  __requires("A: int -> int");
  __consumes("a ~> Matrix1(N, A)");
  __reads("b ~~>gpu 0");
  __produces("a ~> Matrix1(N, add_x(5, A))");

  //__GPU_GET(b);
  *b;

  for (int i = 0; i < N; i++) {
    __xconsumes("&a[MINDEX1(N,i)] ~~> A(i)");
    __xproduces("&a[MINDEX1(N,i)] ~~> add_x(5, A)(i)");

    a[MINDEX1(N,i)] = a[MINDEX1(N, i)] + 5;
  }
}
