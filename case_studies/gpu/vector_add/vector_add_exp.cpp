#include <optitrust_gpu.h>
#include <optitrust_models.h>

__ghost(define,
        "x := fun (A: int -> float) (B: int -> float) -> fun (i: int) -> A(i) "
        "+. B(i)",
        "arr_add <- x");

void vector_add(float* a, float* b, float* c, int N) {
  __requires("A: int -> float");
  __requires("B: int -> float");
  __requires("n_factor: (MSIZE1(N / 256) * MSIZE1(256) = MSIZE1(N))");
  __preserves("HostCtx");
  __writes("c ~> Matrix1(N, arr_add(A, B))");
  __reads("a ~> Matrix1(N, A)");
  __reads("b ~> Matrix1(N, B)");
  float* const d_c = __gmem_malloc1<float>(N);
  __with("T := float");
  __ghost([&]() {
    __preserves("d_c ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  float* const d_a = __gmem_malloc1<float>(N);
  __with("T := float");
  __ghost([&]() {
    __preserves("d_a ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device1(d_a, a, N);
  float* const d_b = __gmem_malloc1<float>(N);
  __with("T := float");
  __ghost([&]() {
    __preserves("d_b ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device1(d_b, b, N);
  /*@kernel_sequence*/ {
    kernel_launch(MSIZE1(N / 256), MSIZE1(256), 0);
    kernel_setup_end();
    __with("grid_sz := MSIZE1(N)");
    __threadfor;
    for (int i = 0; i < N; i++) {
      __sreads("d_a ~> Matrix1Of(N, GMem, A)");
      __sreads("d_b ~> Matrix1Of(N, GMem, B)");
      __xwrites("&d_c[MINDEX1(N, i)] ~~>[GMem] arr_add(A, B)(i)");
      const __ghost_fn __ghost_pair_1 =
          __ghost_begin(desyncro_to_ro, "H := d_a ~> Matrix1Of(N, GMem, A)");
      const __ghost_fn __ghost_pair_2 =
          __ghost_begin(desyncro_to_ro, "H := d_b ~> Matrix1Of(N, GMem, B)");
      {
        const __ghost_fn focusA =
            __ghost_begin(ro_matrix1_focus, "matrix := d_a, i := i");
        const __ghost_fn focusB =
            __ghost_begin(ro_matrix1_focus, "matrix := d_b, i := i");
        const float va = __gmem_get(&d_a[MINDEX1(N, i)]);
        const float vb = __gmem_get(&d_b[MINDEX1(N, i)]);
        __gmem_set(&d_c[MINDEX1(N, i)], va + vb);
        __ghost_end(focusA);
        __ghost_end(focusB);
      }
      __ghost_end(__ghost_pair_1);
      __ghost_end(__ghost_pair_2);
    }
    kernel_teardown_begin();
    __with("grid_sz := MSIZE1(N)");
    __barrier_sequence;
    {
      __ghost(kernel_teardown_sync_dro, "H := d_a ~> Matrix1Of(N, GMem, A)");
      __ghost(kernel_teardown_sync_dro, "H := d_b ~> Matrix1Of(N, GMem, B)");
      __ghost(kernel_teardown_sync,
              "H := desync_for i in ..N -> &d_c[MINDEX1(N, i)] ~~>[GMem] A(i) "
              "+. B(i)");
    }
    kernel_kill();
  } /*kernel_sequence@*/
  __ghost([&]() {
    __preserves("d_b ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_b);
  __ghost([&]() {
    __preserves("d_a ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_a);
  memcpy_device_to_host1(c, d_c, N);
  __ghost([&]() {
    __preserves("d_c ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_c);
}
