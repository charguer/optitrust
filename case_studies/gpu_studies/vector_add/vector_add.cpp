#include <optitrust_models.h>

// TODO how to do it with floats? it won't parse "add_x(5.0, A)"
__DEF(add_x, "fun (x: int) (A: int -> int) -> fun (i : int) -> A(i) + x");


void vector_add(int *a, int *b, int N) {
  __requires("A: int -> int");
  __consumes("a ~> Matrix1(N, A)");
  __produces("a ~> Matrix1(N, add_x(5, A))");

  for (int i = 0; i < N; i++) {
    __xconsumes("&a[MINDEX1(N,i)] ~~> A(i)");
    __xproduces("&a[MINDEX1(N,i)] ~~> add_x(5, A)(i)");

    a[MINDEX1(N,i)] = a[MINDEX1(N, i)] + 5;
  }

}
