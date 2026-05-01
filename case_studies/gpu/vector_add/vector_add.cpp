#include <optitrust_models.h>
#include <optitrust_gpu.h>

__DEF(arr_add, "fun (A: int -> float) (B: int -> float) -> fun (i : int) -> A(i) +. B(i)");

// better names for input/output params in paper (in0,in1/out)
void vector_add(float *a, float *b, float *c, int N) {
  __requires("A: int -> float, B: int -> float");
  __requires("n_factor: MSIZE1(N/256) * MSIZE1(256) = MSIZE1(N)");
  __preserves("HostCtx");
  __reads("a ~> Matrix1(N, A)");             // for i in 0..N -> &a[MINDEX1(N,i)] ~~> A(i)
  __reads("b ~> Matrix1(N, B)");             // for i in 0..N -> &b[MINDEX1(N,i)] ~~> B(i)
  __writes("c ~> Matrix1(N, arr_add(A,B))"); // for i in 0..N -> &c[MINDEX1(N,i)] ~~> (arr_add(A,B))(i)

  for (int i = 0; i < N; i++) {
    __xwrites("&c[MINDEX1(N,i)] ~~> (arr_add(A,B))(i)");

    __GHOST_BEGIN(focusA, ro_matrix1_focus, "a, i");
    __GHOST_BEGIN(focusB, ro_matrix1_focus, "b, i");
    const float va = a[MINDEX1(N,i)];
    const float vb = b[MINDEX1(N,i)];
    c[MINDEX1(N,i)] = va + vb;
    __GHOST_END(focusA);
    __GHOST_END(focusB);
  }
}
