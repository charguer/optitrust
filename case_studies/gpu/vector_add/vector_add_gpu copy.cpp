#include <optitrust_models.h>
#include <optitrust_gpu.h>

__DEF(arr_add, "fun (A: int -> float) (B: int -> float) -> fun (i : int) -> A(i) +. B(i)");
__DEF(rr1, "fun (sz: int) -> MINDEX1(0,0) ..+ sz");

// requires typechecker to have a definition for body of MSIZE (for unification)
// __DEF(divisible, "fun (base: int) (n:int) -> base/n * n = base");

void vector_add(float *a, float *b, float *c, int N) {
  __requires("A: int -> float, B: int -> float");
  __requires("n_factor: N/256 * 256 = MSIZE1(N)");
  __preserves("HostCtx");
  __reads("a ~> Matrix1(N, A)");             // for i in 0..N -> &a[MINDEX1(N,i)] ~~> A(i)
  __reads("b ~> Matrix1(N, B)");             // for i in 0..N -> &b[MINDEX1(N,i)] ~~> B(i)
  __writes("c ~> Matrix1(N, arr_add(A,B))"); // for i in 0..N -> &c[MINDEX1(N,i)] ~~> (arr_add(A,B))(i)

  float* const d_a = GMEM_MALLOC1(float, N);
  float* const d_b = GMEM_MALLOC1(float, N);
  float* const d_c = GMEM_MALLOC1(float, N);

  memcpy_host_to_device1(d_a, a, N);
  memcpy_host_to_device1(d_b, b, N);

  {
    kernel_launch(256, N/256, 0);
    kernel_setup_end();__with("by := n_factor");
    __ghost(group_to_desyncgroup, "N := N, items := fun i -> &d_c[MINDEX1(N,i)] ~> UninitCellOf(GMem)");

    __threadfor; for (int i = 0; i < N; i++) {
      __xwrites("&d_c[MINDEX1(N,i)] ~~>[GMem] (arr_add(A,B))(i)");

      __GHOST_BEGIN(focusA, ro_matrix1_focus, "d_a, i");
      __GHOST_BEGIN(focusB, ro_matrix1_focus, "d_b, i");
      const float va = __gmem_get(&d_a[MINDEX1(N,i)]);
      const float vb = __gmem_get(&d_b[MINDEX1(N,i)]);
      __gmem_set(&d_c[MINDEX1(N,i)], va + vb);
      __GHOST_END(focusA);
      __GHOST_END(focusB);
    }

    kernel_teardown_begin();
    __ghost(kernel_teardown_sync, "H := desync_for i in ..N -> &d_c[MINDEX1(N,i)] ~~>[GMem] (arr_add(A,B))(i)");
    kernel_kill();
  }
  memcpy_device_to_host1(c, d_c, N);

  gmem_free(d_a);
  gmem_free(d_b);
  gmem_free(d_c);
}
