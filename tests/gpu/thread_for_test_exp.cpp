#include "optitrust_gpu.h"
#include "optitrust_models.h"

// NOTE: using pretty matrix notation

__device__ void basic(int* a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 1");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE2(N, M))");
  __reads("KernelParams(bpg, MSIZE2(N, M), smem_sz)");
  __threadfor;
  for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[i][j] ~~>[GMem] 0");
    __xproduces("desync_for j in ..M -> &a[i][j] ~~>[GMem] 1");
    __threadfor;
    for (int j = 0; j < M; j++) {
      __xconsumes("&a[i][j] ~~>[GMem] 0");
      __xproduces("&a[i][j] ~~>[GMem] 1");
      __gmem_set(&a[i][j], 1);
    }
  }
  blocksync();
  __with(
      "H := desync_for i in ..N -> desync_for j in ..M -> &a[i][j] ~~>[GMem] "
      "1");
}

__device__ void retile_desyncgroups(int* a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i * M + j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i * M + j] ~~>[GMem] 1 + 1");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE2(N, M))");
  __reads("KernelParams(bpg, MSIZE2(N, M), smem_sz)");
  __ghost(assume, "P := (MSIZE2(N * M / 32, 32) = N * M)", "eq_retile_1 <- H");
  __ghost(assume, "P := (MSIZE2(N * M / 32, 32) = N * M / 32 * 32)",
          "eq_retile_2 <- H");
  __ghost(assume, "P := (MSIZE2(N, M) = MSIZE2(N * M / 32, 32))",
          "eq_retile_3 <- H");
  __ghost(assume, "P := (32 >= 0)");
  __ghost(assume, "P := (M >= 0)");
  __threadfor;
  for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[i * M + j] ~~>[GMem] 0");
    __xproduces("desync_for j in ..M -> &a[i * M + j] ~~>[GMem] 1");
    __threadfor;
    for (int j = 0; j < M; j++) {
      __xconsumes("&a[i * M + j] ~~>[GMem] 0");
      __xproduces("&a[i * M + j] ~~>[GMem] 1");
      __gmem_set(&a[i * M + j], 1);
    }
  }
  __ghost(desync_untile_divides,
          "items := fun k -> &a[k] ~~>[GMem] 1, div_check := eq_retile_1");
  __ghost(desync_tile_divides,
          "items := fun k -> &a[k] ~~>[GMem] 1, div_check := eq_retile_2");
  __ghost(rewrite_threadsctx_sz, "by := eq_retile_3");
  __threadfor;
  for (int i = 0; i < N * M / 32; i++) {
    __xconsumes("desync_for j in ..32 -> &a[i * 32 + j] ~~>[GMem] 1");
    __xproduces("desync_for j in ..32 -> &a[i * 32 + j] ~~>[GMem] 1 + 1");
    __threadfor;
    for (int j = 0; j < 32; j++) {
      __xconsumes("&a[i * 32 + j] ~~>[GMem] 1");
      __xproduces("&a[i * 32 + j] ~~>[GMem] 1 + 1");
      const int va = __gmem_get(&a[i * 32 + j]);
      __gmem_set(&a[i * 32 + j], va + 1);
    }
  }
  __ghost(desync_untile_divides,
          "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_2");
  __ghost(desync_tile_divides,
          "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_1");
  __ghost(rewrite_threadsctx_sz,
          "by := eq_sym(MSIZE2(N, M), MSIZE2(N * M / 32, 32), eq_retile_3)");
  blocksync();
  __with(
      "H := desync_for i in ..N -> desync_for j in ..M -> &a[i * M + j] "
      "~~>[GMem] 1 + 1");
}

__device__ void sync_required(int* a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 1 + 1");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE2(N, M))");
  __reads("KernelParams(bpg, MSIZE2(N, M), smem_sz)");
  __ghost(assume, "P := (MSIZE2(N, M) = MSIZE2(M, N))", "msize_commute <- H");
  __threadfor;
  for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[i][j] ~~>[GMem] 0");
    __xproduces("desync_for j in ..M -> &a[i][j] ~~>[GMem] 1");
    __threadfor;
    for (int j = 0; j < M; j++) {
      __xconsumes("&a[i][j] ~~>[GMem] 0");
      __xproduces("&a[i][j] ~~>[GMem] 1");
      __gmem_set(&a[i][j], 1);
    }
  }
  blocksync();
  __with(
      "H := desync_for i in ..N -> desync_for j in ..M -> &a[i][j] ~~>[GMem] "
      "1");
  __ghost(swap_groups, "items := fun i j -> &a[i][j] ~~>[GMem] 1");
  __ghost(rewrite_threadsctx_sz, "by := msize_commute");
  __threadfor;
  for (int i = 0; i < M; i++) {
    __xconsumes("for j in 0..N -> &a[j][i] ~~>[GMem] 1");
    __xproduces("desync_for j in ..N -> &a[j][i] ~~>[GMem] 1 + 1");
    __threadfor;
    for (int j = 0; j < N; j++) {
      __xconsumes("&a[j][i] ~~>[GMem] 1");
      __xproduces("&a[j][i] ~~>[GMem] 1 + 1");
      __gmem_set(&a[j][i], __gmem_get(&a[j][i]) + 1);
    }
  }
  __ghost(rewrite_threadsctx_sz,
          "by := eq_sym(MSIZE2(N, M), MSIZE2(M, N), msize_commute)");
  blocksync();
  __with(
      "H := desync_for i in ..M -> desync_for j in ..N -> &a[j][i] ~~>[GMem] 1 "
      "+ 1");
  __ghost(swap_groups, "items := fun i j -> &a[j][i] ~~>[GMem] 1 + 1");
}

__device__ void write_test1(int* a, int N) {
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[i] ~~>[GMem] 1");
  __threadfor;
  for (int i = 0; i < N; i++) {
    __xwrites("&a[i] ~~>[GMem] 1");
    __gmem_set(&a[i], 1);
  }
}

__device__ void write_test2(int* a, int N) {
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> &a[i] ~~>[GMem] 0");
  __produces("for i in 0..N -> &a[i] ~~>[GMem] 1");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE1(N))");
  __reads("KernelParams(bpg, MSIZE1(N), smem_sz)");
  write_test1(a, N);
  blocksync();
  __with("H := desync_for i in ..N -> &a[i] ~~>[GMem] 1");
}

__ghost(assert_inhabited, "x := arbitrary(int * (int -> int) -> int)",
        "reduce_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> int) -> (0 = reduce_sum(0, f)))",
        "reduce_sum_empty <- proof");

__ghost(assert_prop,
        "proof := admit(forall (n: int) (f: int -> int) (_: (n >= 0)) -> "
        "(reduce_sum(n, f) + f(n) = reduce_sum(n + 1, f)))",
        "reduce_sum_add_right <- proof");

__device__ void read_thread_outer(int* a, int* b, int N) {
  __requires("B: int -> int");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __reads("b ~> Matrix1Of(N, GMem, B)");
  __threadfor;
  for (int t = 0; t < N; t++) {
    __xwrites("&a[t] ~~>[GMem] reduce_sum(N, B)");
    __gmem_set(&a[t], 0);
    __ghost(rewrite_linear,
            "inside := fun v -> &a[t] ~~>[GMem] v, by := reduce_sum_empty(B)");
    for (int i = 0; i < N; i++) {
      __spreserves("&a[t] ~~>[GMem] reduce_sum(i, B)");
      const __ghost_fn focus =
          __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
      const int va = __gmem_get(&a[t]);
      const int vb = __gmem_get(&b[i]);
      __gmem_set(&a[t], va + vb);
      __ghost_end(focus);
      __ghost(in_range_bounds, "x := i", "i_geq_0 <- lower_bound");
      __ghost(rewrite_linear,
              "inside := fun v -> &a[t] ~~>[GMem] v, by := "
              "reduce_sum_add_right(i, B, i_geq_0)");
    }
  }
}

__device__ void read_thread_inner(int* a, int* b, int N) {
  __requires("B: int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __preserves("ThreadsCtx(MINDEX1(0, 0)..+MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __reads("KernelParams(bpg, MSIZE1(N), smem_sz)");
  __reads("b ~> Matrix1Of(N, GMem, B)");
  __threadfor;
  for (int t = 0; t < N; t++) {
    __xwrites("&a[t] ~~>[GMem] reduce_sum(0, B)");
    __gmem_set(&a[t], 0);
    __ghost(rewrite_linear,
            "inside := fun v -> &a[t] ~~>[GMem] v, by := reduce_sum_empty(B)");
  }
  for (int i = 0; i < N; i++) {
    __spreserves("desync_for t in ..N -> &a[t] ~~>[GMem] reduce_sum(i, B)");
    __threadfor;
    for (int t = 0; t < N; t++) {
      __xconsumes("&a[t] ~~>[GMem] reduce_sum(i, B)");
      __xproduces("&a[t] ~~>[GMem] reduce_sum(i + 1, B)");
      const __ghost_fn focus =
          __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
      const int va = __gmem_get(&a[t]);
      const int vb = __gmem_get(&b[i]);
      __gmem_set(&a[t], va + vb);
      __ghost_end(focus);
      __ghost(in_range_bounds, "x := i", "i_geq_0 <- lower_bound");
      __ghost(rewrite_linear,
              "inside := fun v -> &a[t] ~~>[GMem] v, by := "
              "reduce_sum_add_right(i, B, i_geq_0)");
    }
  }
}
