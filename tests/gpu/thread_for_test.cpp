#include "optitrust_models.h"
#include "optitrust_gpu.h"


__device;
void test1(int *a, int N, int M) {
  __requires("N: int, M: int, A: int * int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(MSIZE2(N,M), bpg, smem_sz)");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(N,M),0), MSIZE2(N,M)))");
  __consumes("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
  __ghost(group_to_desyncgroup2, "items := fun i j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");

  __DEF(r1, "range_plus(MINDEX1(MSIZE2(N,M),0), MSIZE2(N,M))");
  __DEF(r2, "fun (i: int) -> range_plus(MINDEX2(N,MSIZE1(M),i,0), MSIZE1(M))");

  __threadfor; for (int i = 0; i < N; i++) {
    __xconsumes("desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    __xproduces("desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __GMEM_SET(&a[MINDEX2(N,M,i,j)],1);
    }
  }
  blocksync(); __with("H := desync_for(r1) i in ..N -> desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
}


__device;
void test2(int *a, int N, int M) {
  __requires("N: int, M: int, A: int * int -> int");
  __requires("eq_retile: MSIZE2(N,M) = MSIZE2(N*M/32, 32)");
  __requires("32 >= 0, M >= 0");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(MSIZE2(N,M), bpg, smem_sz)");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(N,M),0), MSIZE2(N,M)))");
  __consumes("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1 + 1");
  __ghost(group_to_desyncgroup2, "items := fun i j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");

  __DEF(r1, "fun (sz: int) -> range_plus(MINDEX1(sz,0), sz)");
  __DEF(r2, "fun (i: int) -> range_plus(MINDEX2(N,MSIZE1(M),i,0), MSIZE1(M))");
  __DEF(r3, "fun (i: int) -> range_plus(MINDEX2(N*M/32,MSIZE1(32),i,0), MSIZE1(32))");

  __threadfor; for (int i = 0; i < N; i++) {
    __xconsumes("desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    __xproduces("desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __GMEM_SET(&a[MINDEX2(N,M,i,j)], 1);
    }
  }
  __ghost(rewrite_range, "rf := r1, from := MSIZE2(N,M), to := MSIZE2(N*M/32, 32)");
  __ghost(desyncgroup_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1,  div_check := eq_retile");
  __ghost(chunk_range_plus2, "D2 := N*M/32, D1 := 32");
  __ghost(desyncgroup_tile_divides, "items := fun i -> &a[i] ~~>[GMem] 1, tile_count := N*M/32, tile_size := 32");

  __ghost(rewrite_linear_range, "from := r1(MSIZE2(N,M)), to := r1(MSIZE2(N*M/32, 32)), inside := fun (r: Range) -> ThreadsCtx(r)");

  __threadfor; for (int i = 0; i < N*M/32; i++) {
    __xconsumes("DesyncGroup(r3(i), 32, fun j -> &a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1)");
    __xproduces("DesyncGroup(r3(i), 32, fun j -> &a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1+1)");
    __threadfor; for (int j = 0; j < 32; j++) {
      __xconsumes("&a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1");
      __xproduces("&a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1+1");
      __GMEM_SET(&a[MINDEX2(N*M/32,32,i,j)], __GMEM_GET(&a[MINDEX2(N*M/32,32,i,j)]) + 1);
    }
  }

  __PROOF_OF(eq_retile_sym, "MSIZE2(N*M/32, 32) = MSIZE2(N,M)", "eq_sym(MSIZE2(N,M), MSIZE2(N*M/32, 32), eq_retile)");

  __ghost(rewrite_range, "rf := r1, from := MSIZE2(N*M/32, 32), to := MSIZE2(N,M)");
  __ghost(desyncgroup_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_sym");
  __ghost(chunk_range_plus2, "D2 := N, D1 := M");
  __ghost(desyncgroup_tile_divides, "items := fun i -> &a[i] ~~>[GMem] 1 + 1, tile_count := N, tile_size := M");

  __ghost(rewrite_linear_range, "from := r1(MSIZE2(N*M/32, 32)), to := r1(MSIZE2(N,M)), inside := fun (r: Range) -> ThreadsCtx(r)");

  blocksync(); __with("H := desync_for(r1(MSIZE2(N,M))) i in ..N -> desync_for(r2(i)) j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1+1");
}

__DEF(rr1, "fun (sz: int) -> range_plus(MINDEX1(MSIZE1(sz),0), MSIZE1(sz))");

__device;
void write_test1(int *a, int N) {
  __preserves("ThreadsCtx(rr1(N))");
  __writes("desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] 1");

  __threadfor; for (int i = 0; i < N; i++) {
    __xwrites("&a[i] ~~>[GMem] 1");
    __GMEM_SET(&a[i], 1);
  }
}

__device;
void write_test2(int *a, int N) {
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(MSIZE1(N), bpg, smem_sz)");
  __preserves("ThreadsCtx(rr1(N))");
  __consumes("for i in 0..N -> &a[i] ~~>[GMem] 0");
  __produces("for i in 0..N -> &a[i] ~~>[GMem] 1");

  __ghost(group_to_desyncgroup, "items := fun i -> &a[i] ~~>[GMem]0");

  write_test1(a, N);
  blocksync(); __with("H := desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] 1");
}

__DECL(reduce_sum, "int * (int -> int) -> int");
__AXIOM(reduce_sum_empty, "forall (f: int -> int) -> 0 = reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> int) (_: n >= 0) -> reduce_sum(n, f) + f(n) = reduce_sum(n + 1, f)");

__device;
void read_test1(int *a, int *b, int N) {
  __requires("B: int -> int");
  __preserves("ThreadsCtx(rr1(N))");
  __writes("desync_for(rr1(N)) i in ..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __reads("for i in 0..N -> &b[MINDEX1(N,i)] ~~>[GMem] B(i)");

  __threadfor; for (int i = 0; i < N; i++) {
    __xwrites("&a[i] ~~>[GMem] reduce_sum(N, B)");
    __GMEM_SET(&a[i],0);
    __ghost(rewrite_linear, "inside := (fun v -> &a[i] ~~>[GMem] v), by := reduce_sum_empty(B)");
    for (int j = 0; j < N; j++) {
      __spreserves("&a[i] ~~>[GMem] reduce_sum(j, B)");
      __GHOST_BEGIN(focus, ro_matrix1_focus_generic, "b, j");
      const int v = __GMEM_GET(&b[MINDEX1(N,j)]);
      __GMEM_SET(&a[i], __GMEM_GET(&a[i]) + v); // TODO: contracts still won't allow multiple __GMEM_GETs because of ThreadsCtx
      __GHOST_END(focus);
      __ghost(in_range_bounds, "j", "j_geq_0 <- lower_bound");
      __ghost(rewrite_linear, "inside := (fun v -> &a[i] ~~>[GMem] v), by := reduce_sum_add_right(j, B, j_geq_0)");
    }
  }
}

__device;
void read_test2(int *a, int *b, int N) {
  __requires("B: int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(MSIZE1(N), bpg, smem_sz)");
  __preserves("ThreadsCtx(rr1(N))");
  __writes("for i in 0..N -> &a[MINDEX1(N,i)] ~~>[GMem] reduce_sum(N,B)");
  __reads("for i in 0..N -> &b[MINDEX1(N,i)] ~~>[GMem] B(i)");

  __ghost(group_to_desyncgroup, "items := fun i -> &a[MINDEX1(N,i)] ~> UninitCellOf(GMem)");

  __threadfor; for (int t = 0; t < N; t++) {
    __xwrites("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(0, B)");
    __GMEM_SET(&a[MINDEX1(N,t)], 0);
    __ghost(rewrite_linear, "inside := (fun v -> &a[MINDEX1(N,t)] ~~>[GMem] v), by := reduce_sum_empty(B)");
  }

  blocksync(); __with("H := desync_for(rr1(N)) i in ..N -> &a[MINDEX1(N,i)] ~~>[GMem] reduce_sum(0,B)");

  // TODO: is this right? shouldn't the outputs shift every iteration
  for (int i = 0; i < N; i++) {
    __spreserves("for j in 0..N -> &a[MINDEX1(N,j)] ~~>[GMem] reduce_sum(i, B)");
    __ghost(group_to_desyncgroup, "items := fun j -> &a[MINDEX1(N,j)] ~~>[GMem] reduce_sum(i,B)");
    __threadfor; for (int t = 0; t < N; t++) {
      __xconsumes("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(i, B)");
      __xproduces("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(i+1, B)");
      __GHOST_BEGIN(focus, ro_matrix1_focus_generic, "b, i");
      const int v = __GMEM_GET(&a[MINDEX1(N,t)]);
      __GMEM_SET(&a[MINDEX1(N,t)], v + __GMEM_GET(&b[MINDEX1(N,i)]));
      __GHOST_END(focus);
      __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
      __ghost(rewrite_linear, "inside := (fun v -> &a[MINDEX1(N,t)] ~~>[GMem] v), by := reduce_sum_add_right(i, B, i_geq_0)");
    }
    blocksync(); __with("H := desync_for(rr1(N)) j in ..N -> &a[MINDEX1(N,j)] ~~>[GMem] reduce_sum(i+1,B)");
  }
}
