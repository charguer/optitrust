#include <optitrust_models.h>
#include <optitrust_gpu.h>

__DEF(arr_add, "fun (A: int -> float) (B: int -> float) -> fun (i : int) -> A(i) +. B(i)");
__DEF(rr1, "fun (sz: int) -> range_plus(MINDEX1(sz,0), sz)");

void vector_add(float *a, float *b, float *c, int N) {
  __requires("A: int -> float, B: int -> float");
  __requires("n_factor: N/256 * 256 = MSIZE1(N)");
  __preserves("HostCtx");
  __reads("for i in 0..N -> &a[MINDEX1(N,i)] ~~> A(i)");
  __reads("for i in 0..N -> &b[MINDEX1(N,i)] ~~> B(i)");
  __writes("for i in 0..N -> &c[MINDEX1(N,i)] ~~> (arr_add(A,B))(i)");

  float* const d_a = gmem_malloc1(float, N);
  float* const d_b = gmem_malloc1(float, N);
  float* const d_c = gmem_malloc1(float, N);

  memcpy_host_to_device1(d_a, a, N);
  memcpy_host_to_device1(d_b, b, N);

  __ghost(rewrite_range, "rf := rr1, by := n_factor");
  kernel_start(256, N/256, 0);__with("r := rr1(MSIZE1(N))");
  __ghost(group_to_desyncgroup, "N := N, items := fun i -> &d_c[MINDEX1(N,i)] ~> UninitCellOf(GMem)");

  __threadfor; for (int i = 0; i < N; i++) {
    __xwrites("&d_c[MINDEX1(N,i)] ~~>[GMem] (arr_add(A,B))(i)");

    __GHOST_BEGIN(focusA, ro_matrix1_focus_generic, "d_a, i");
    __GHOST_BEGIN(focusB, ro_matrix1_focus_generic, "d_b, i");
    const float va = __GMEM_GET(&d_a[MINDEX1(N,i)]);
    const float vb = __GMEM_GET(&d_b[MINDEX1(N,i)]);
    __GMEM_SET(&d_c[MINDEX1(N,i)], va + vb);
    __GHOST_END(focusA);
    __GHOST_END(focusB);
  }

  __ghost(kernel_end_sync, "by := n_factor, H := desync_for(rr1(MSIZE1(N))) i in ..N -> &d_c[MINDEX1(N,i)] ~~>[GMem] (arr_add(A,B))(i)");

  kernel_end();

  memcpy_device_to_host1(c, d_c, N);

  gmem_free(d_a);
  gmem_free(d_b);
  gmem_free(d_c);
}
