#include <optitrust_models.h>
#include <optitrust_gpu.h>

__DEF(add_x, "fun (x: float) (A: int -> float) -> fun (i : int) -> A(i) +. x");

void vector_add(float *a, float *b, int N) {
  __requires("A: int -> float, t: int");
  __preserves("ThreadsCtx(range_plus(t,MSIZE0()))");
  __consumes("for i in 0..N -> &a[MINDEX1(N,i)] ~~>[GMem] A(i)");
  __produces("for i in 0..N -> &a[MINDEX1(N,i)] ~~>[GMem] (add_x(5.f,A))(i)");

  for (int i = 0; i < N; i++) {
    __xconsumes("&a[MINDEX1(N,i)] ~~>[GMem] A(i)");
    __xproduces("&a[MINDEX1(N,i)] ~~>[GMem] add_x(5.f, A)(i)");

    // TODO producing weird errors about
    // Arithmetic operand has a non standard type (Trm_var(float))")
    // if I write the + 5.f here
    const float v = __GMEM_GET(&a[MINDEX1(N, i)]);
    __GMEM_SET(&a[MINDEX1(N,i)], v + 5.f);
  }

}
