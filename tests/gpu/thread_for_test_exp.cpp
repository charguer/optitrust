#include "optitrust_gpu.h"
#include "optitrust_models.h"

// NOTE: using pretty matrix notation

void test1(int* a, int N, int M) {
  __requires("N: int");
  __requires("M: int");
  __requires("A: int * int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 1");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(N, M), 0), MSIZE2(N, M)))");
  __reads("KernelParams(MSIZE2(N, M), bpg, smem_sz)");
  __ghost(group_to_desyncgroup2, "items := fun i j -> &a[i][j] ~~>[GMem] 0");
  __ghost(define, "x := range_plus(MINDEX1(MSIZE2(N, M), 0), MSIZE2(N, M))",
          "r1 <- x");
  __ghost(
      define,
      "x := fun (i: int) -> range_plus(MINDEX2(N, MSIZE1(M), i, 0), MSIZE1(M))",
      "r2 <- x");
  thread for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("desync_for(r2(i)) j in ..M -> &a[i][j] ~~>[GMem] 0");
    __xproduces("desync_for(r2(i)) j in ..M -> &a[i][j] ~~>[GMem] 1");
    thread for (int j = 0; j < M; j++) {
      __strict();
      __xconsumes("&a[i][j] ~~>[GMem] 0");
      __xproduces("&a[i][j] ~~>[GMem] 1");
      __gmem_set(&a[i][j], 1);
    }
  }
  blocksync();
  __with(
      "H := desync_for(r1) i in ..N -> desync_for(r2(i)) j in ..M -> &a[i][j] "
      "~~>[GMem] 1");
}

void test2(int* a, int N, int M) {
  __requires("N: int");
  __requires("M: int");
  __requires("A: int * int -> int");
  __requires("eq_retile: __is_true(MSIZE2(N, M) == MSIZE2(N * M / 32, 32))");
  __requires("__is_true(32 >= 0)");
  __requires("__is_true(M >= 0)");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i][j] ~~>[GMem] 1 + 1");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(N, M), 0), MSIZE2(N, M)))");
  __reads("KernelParams(MSIZE2(N, M), bpg, smem_sz)");
  __ghost(group_to_desyncgroup2, "items := fun i j -> &a[i][j] ~~>[GMem] 0");
  __ghost(define, "x := fun (sz: int) -> range_plus(MINDEX1(sz, 0), sz)",
          "r1 <- x");
  __ghost(
      define,
      "x := fun (i: int) -> range_plus(MINDEX2(N, MSIZE1(M), i, 0), MSIZE1(M))",
      "r2 <- x");
  __ghost(define,
          "x := fun (i: int) -> range_plus(MINDEX2(N * M / 32, MSIZE1(32), i, "
          "0), MSIZE1(32))",
          "r3 <- x");
  thread for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("desync_for(r2(i)) j in ..M -> &a[i][j] ~~>[GMem] 0");
    __xproduces("desync_for(r2(i)) j in ..M -> &a[i][j] ~~>[GMem] 1");
    thread for (int j = 0; j < M; j++) {
      __strict();
      __xconsumes("&a[i][j] ~~>[GMem] 0");
      __xproduces("&a[i][j] ~~>[GMem] 1");
      __gmem_set(&a[i][j], 1);
    }
  }
  __ghost(rewrite_range,
          "rf := r1, from := MSIZE2(N, M), to := MSIZE2(N * M / 32, 32)");
  __ghost(desyncgroup_untile_divides,
          "items := fun k -> &a[k] ~~>[GMem] 1, div_check := eq_retile");
  __ghost(chunk_range_plus2, "D2 := N * M / 32, D1 := 32");
  __ghost(desyncgroup_tile_divides,
          "items := fun i -> &a[i] ~~>[GMem] 1, tile_count := N * M / 32, "
          "tile_size := 32");
  __ghost(rewrite_linear_range,
          "from := r1(MSIZE2(N, M)), to := r1(MSIZE2(N * M / 32, 32)), inside "
          ":= fun (r: Range) -> ThreadsCtx(r)");
  thread for (int i = 0; i < N * M / 32; i++) {
    __strict();
    __xconsumes("desync_for(r3(i)) j in ..32 -> &a[i][j] ~~>[GMem] 1");
    __xproduces("desync_for(r3(i)) j in ..32 -> &a[i][j] ~~>[GMem] 1 + 1");
    thread for (int j = 0; j < 32; j++) {
      __strict();
      __xconsumes("&a[i][j] ~~>[GMem] 1");
      __xproduces("&a[i][j] ~~>[GMem] 1 + 1");
      const int va = __gmem_get(&a[i][j]);
      __gmem_set(&a[i][j], va + 1);
    }
  }
  __ghost(assert_prop,
          "P := __is_true(MSIZE2(N * M / 32, 32) == MSIZE2(N, M)), proof := "
          "eq_sym(MSIZE2(N, M), MSIZE2(N * M / 32, 32), eq_retile)",
          "eq_retile_sym <- proof");
  __ghost(rewrite_range,
          "rf := r1, from := MSIZE2(N * M / 32, 32), to := MSIZE2(N, M)");
  __ghost(
      desyncgroup_untile_divides,
      "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_sym");
  __ghost(chunk_range_plus2, "D2 := N, D1 := M");
  __ghost(desyncgroup_tile_divides,
          "items := fun i -> &a[i] ~~>[GMem] 1 + 1, tile_count := N, tile_size "
          ":= M");
  __ghost(rewrite_linear_range,
          "from := r1(MSIZE2(N * M / 32, 32)), to := r1(MSIZE2(N, M)), inside "
          ":= fun (r: Range) -> ThreadsCtx(r)");
  blocksync();
  __with(
      "H := desync_for(r1(MSIZE2(N, M))) i in ..N -> desync_for(r2(i)) j in "
      "..M -> &a[i][j] ~~>[GMem] 1 + 1");
}

__ghost(define,
        "x := fun (sz: int) -> range_plus(MINDEX1(MSIZE1(sz), 0), MSIZE1(sz))",
        "rr1 <- x");

void write_test1(int* a, int N) {
  __preserves("ThreadsCtx(rr1(N))");
  __writes("desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] 1");
  thread for (int i = 0; i < N; i++) {
    __strict();
    __xwrites("&a[i] ~~>[GMem] 1");
    __gmem_set(&a[i], 1);
  }
}

void write_test2(int* a, int N) {
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("for i in 0..N -> &a[i] ~~>[GMem] 0");
  __produces("for i in 0..N -> &a[i] ~~>[GMem] 1");
  __preserves("ThreadsCtx(rr1(N))");
  __reads("KernelParams(MSIZE1(N), bpg, smem_sz)");
  __ghost(group_to_desyncgroup, "items := fun i -> &a[i] ~~>[GMem] 0");
  write_test1(a, N);
  blocksync();
  __with("H := desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] 1");
}

__ghost(assert_inhabited, "x := arbitrary(int * (int -> int) -> int)",
        "reduce_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> int) -> __is_true(0 == reduce_sum(0, "
        "f)))",
        "reduce_sum_empty <- proof");

__ghost(assert_prop,
        "proof := admit(forall (n: int) (f: int -> int) (_: __is_true(n >= 0)) "
        "-> __is_true(reduce_sum(n, f) + f(n) == reduce_sum(n + 1, f)))",
        "reduce_sum_add_right <- proof");

void read_test1(int* a, int* b, int N) {
  __requires("B: int -> int");
  __preserves("ThreadsCtx(rr1(N))");
  __writes("desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __reads("b ~> Matrix1(N, B)");
  thread for (int i = 0; i < N; i++) {
    __strict();
    __sreads("b ~> Matrix1(N, B)");
    __xwrites("&a[i] ~~>[GMem] reduce_sum(N, B)");
    __gmem_set(&a[i], 0);
    __ghost(rewrite_linear,
            "inside := fun v -> &a[i] ~~>[GMem] v, by := reduce_sum_empty(B)");
    for (int j = 0; j < N; j++) {
      __strict();
      __spreserves(
          "ThreadsCtx(range_plus(MINDEX2(N, MSIZE0(), i, 0), MSIZE0()))");
      __spreserves("&a[i] ~~>[GMem] reduce_sum(j, B)");
      __sreads("b ~> Matrix1(N, B)");
      const __ghost_fn focus =
          __ghost_begin(ro_matrix1_focus, "matrix := b, i := j");
      const int va = __gmem_get(&a[i]);
      const int vb = __gmem_get(&b[j]);
      __gmem_set(&a[i], va + vb);
      __ghost_end(focus);
      __ghost(in_range_bounds, "x := j", "j_geq_0 <- lower_bound");
      __ghost(rewrite_linear,
              "inside := fun v -> &a[i] ~~>[GMem] v, by := "
              "reduce_sum_add_right(j, B, j_geq_0)");
    }
  }
}

void read_test2(int* a, int* b, int N) {
  __requires("B: int -> int");
  __requires("bpg: int");
  __requires("smem_sz: int");
  __consumes("a ~> UninitMatrix1(N)");
  __produces("for i in 0..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __preserves("ThreadsCtx(rr1(N))");
  __reads("KernelParams(MSIZE1(N), bpg, smem_sz)");
  __reads("b ~> Matrix1(N, B)");
  __ghost(group_to_desyncgroup,
          "items := fun i -> &a[i] ~> UninitCellOf(GMem)");
  thread for (int t = 0; t < N; t++) {
    __strict();
    __xwrites("&a[t] ~~>[GMem] reduce_sum(0, B)");
    __gmem_set(&a[t], 0);
    __ghost(rewrite_linear,
            "inside := fun v -> &a[t] ~~>[GMem] v, by := reduce_sum_empty(B)");
  }
  blocksync();
  __with(
      "H := desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] reduce_sum(0, B)");
  for (int i = 0; i < N; i++) {
    __strict();
    __spreserves("ThreadsCtx(range_plus(MINDEX1(MSIZE1(N), 0), MSIZE1(N)))");
    __spreserves("for j in 0..N -> &a[j] ~~>[GMem] reduce_sum(i, B)");
    __sreads("KernelParams(MSIZE1(N), bpg, smem_sz)");
    __sreads("b ~> Matrix1(N, B)");
    __ghost(group_to_desyncgroup,
            "items := fun j -> &a[j] ~~>[GMem] reduce_sum(i, B)");
    thread for (int t = 0; t < N; t++) {
      __strict();
      __sreads("b ~> Matrix1(N, B)");
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
    blocksync();
    __with(
        "H := desync_for(rr1(N)) j in ..N -> &a[j] ~~>[GMem] reduce_sum(i + 1, "
        "B)");
  }
}
