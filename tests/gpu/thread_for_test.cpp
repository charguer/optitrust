#include "optitrust_models.h"
#include "optitrust_gpu.h"

__device;
void basic(int *a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(bpg, MSIZE2(N,M), smem_sz)");
  __preserves("ThreadsCtx( MINDEX1(0,0) ..+ MSIZE2(N,M) )");
  __consumes("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");

  //__ghost(group_to_desyncgroup, "items := fun i -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");

  //__threadfor; for (int i = 0; i < N; i++) {
  for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    //__xproduces("desync_for j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
    __xproduces("for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");

    //__ghost(group_to_desyncgroup, "items := fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    //__threadfor; for (int j = 0; j < M; j++) {
    for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __gmem_set(&a[MINDEX2(N,M,i,j)],1);
    }
  }
  blocksync(); __with("H := desync_for i in ..N -> desync_for j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
}

__device;
void retile_desyncgroups(int *a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(bpg, MSIZE2(N,M), smem_sz)");
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE2(N,M))");
  __consumes("for i in 0..N -> for j in 0..M -> &a[i * M + j] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[i * M + j] ~~>[GMem] 1 + 1");

  __ghost(assume, "P := MSIZE2(N*M/32, 32) = N * M", "eq_retile_1 <- H");
  __ghost(assume, "P := MSIZE2(N*M/32, 32) = (N*M/32) * 32", "eq_retile_2 <- H");
  __ghost(assume, "P := MSIZE2(N,M) = MSIZE2(N*M/32,32)", "eq_retile_3 <- H");
  __ghost(assume, "32 >= 0");
  __ghost(assume, "M >= 0");

  __ghost(group_to_desyncgroup, "items := fun i -> for j in 0..M -> &a[i * M + j] ~~>[GMem] 0");
  __threadfor; for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[i * M + j] ~~>[GMem] 0");
    __xproduces("desync_for j in ..M -> &a[i * M + j] ~~>[GMem] 1");

    __ghost(group_to_desyncgroup, "items := fun j -> &a[i * M + j] ~~>[GMem] 0");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[i * M + j] ~~>[GMem] 0");
      __xproduces("&a[i * M + j] ~~>[GMem] 1");
      __gmem_set(&a[i * M + j], 1);
    }
  }

  __ghost(desync_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1,  div_check := eq_retile_1");
  __ghost(desync_tile_divides, "items := fun k -> &a[k] ~~>[GMem] 1,  div_check := eq_retile_2");
  __ghost(rewrite_threadsctx_sz, "by := eq_retile_3");

  __threadfor; for (int i = 0; i < N*M/32; i++) {
    __xconsumes("desync_for j in ..32 -> &a[i * 32 + j] ~~>[GMem] 1");
    __xproduces("desync_for j in ..32 -> &a[i * 32 + j] ~~>[GMem] 1+1");
    __threadfor; for (int j = 0; j < 32; j++) {
      __xconsumes("&a[i * 32 + j] ~~>[GMem] 1");
      __xproduces("&a[i * 32 + j] ~~>[GMem] 1+1");
      const int va = __gmem_get(&a[i * 32 + j]);
      __gmem_set(&a[i * 32 + j], va + 1);
    }
  }

  __ghost(desync_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_2");
  __ghost(desync_tile_divides, "items := fun k -> &a[k] ~~>[GMem] 1 + 1, div_check := eq_retile_1");
  __ghost(rewrite_threadsctx_sz, "by := eq_sym(MSIZE2(N,M), MSIZE2(N*M/32,32), eq_retile_3)");

  blocksync(); __with("H := desync_for i in ..N -> desync_for j in ..M -> &a[i * M + j] ~~>[GMem] 1+1");
}

__device;
void sync_required(int *a, int N, int M) {
  __requires("A: int * int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(bpg, MSIZE2(N,M), smem_sz)");
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE2(N,M))");
  __consumes("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1 + 1");

  __ghost(assume, "P := MSIZE2(N,M) = MSIZE2(M,N)", "msize_commute <- H");

  __ghost(group_to_desyncgroup, "items := fun i -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __threadfor; for (int i = 0; i < N; i++) {
    __xconsumes("for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    __xproduces("desync_for j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");

    __ghost(group_to_desyncgroup, "items := fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __gmem_set(&a[MINDEX2(N,M,i,j)], 1);
    }
  }

  blocksync(); __with("H := desync_for i in ..N -> desync_for j in ..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
  __ghost(swap_groups, "items := fun i j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
  __ghost(rewrite_threadsctx_sz, "by := msize_commute");

  __ghost(group_to_desyncgroup, "items := fun j -> for i in 0..N -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
  __threadfor; for (int i = 0; i < M; i++) {
    __xconsumes("for j in 0..N -> &a[MINDEX2(N,M,j,i)] ~~>[GMem] 1");
    __xproduces("desync_for j in ..N -> &a[MINDEX2(N,M,j,i)] ~~>[GMem] 1+1");

    __ghost(group_to_desyncgroup, "items := fun j -> &a[MINDEX2(N,M,j,i)] ~~>[GMem] 1");
    __threadfor; for (int j = 0; j < N; j++) {
      __xconsumes("&a[MINDEX2(N,M,j,i)] ~~>[GMem] 1");
      __xproduces("&a[MINDEX2(N,M,j,i)] ~~>[GMem] 1+1");
      const int va = __gmem_get(&a[MINDEX2(N,M,j,i)]);
      __gmem_set(&a[MINDEX2(N,M,j,i)], va + 1);
    }
  }

  __ghost(rewrite_threadsctx_sz, "by := eq_sym(MSIZE2(N,M), MSIZE2(M,N), msize_commute)");
  blocksync(); __with("H := desync_for i in ..M -> desync_for j in ..N -> &a[MINDEX2(N,M,j,i)] ~~>[GMem] 1+1");
  __ghost(swap_groups, "items := fun i j -> &a[MINDEX2(N,M,j,i)] ~~>[GMem] 1+1");
}

__device;
void write_test1(int *a, int N) {
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[i] ~~>[GMem] 1");

  __threadfor; for (int i = 0; i < N; i++) {
    __xwrites("&a[i] ~~>[GMem] 1");
    __gmem_set(&a[i], 1);
  }
}

__device;
void write_test2(int *a, int N) {
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(bpg, MSIZE1(N), smem_sz)");
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE1(N))");
  __consumes("for i in 0..N -> &a[i] ~~>[GMem] 0");
  __produces("for i in 0..N -> &a[i] ~~>[GMem] 1");

  __ghost(group_to_desyncgroup, "items := fun i -> &a[i] ~~>[GMem]0");

  __device_call; write_test1(a, N);
  blocksync(); __with("H := desync_for i in ..N -> &a[i] ~~>[GMem] 1");
}

__DECL(reduce_sum, "int * (int -> int) -> int");
__AXIOM(reduce_sum_empty, "forall (f: int -> int) -> 0 = reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> int) (_: n >= 0) -> reduce_sum(n, f) + f(n) = reduce_sum(n + 1, f)");

__device;
void read_thread_outer(int *a, int *b, int N) {
  __requires("B: int -> int");
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[i] ~~>[GMem] reduce_sum(N, B)");
  __reads("for i in 0..N -> &b[MINDEX1(N,i)] ~~>[GMem] B(i)");

  __threadfor; for (int t = 0; t < N; t++) {
    __xwrites("&a[t] ~~>[GMem] reduce_sum(N, B)");
    __gmem_set(&a[t],0);
    __ghost(rewrite_linear, "inside := (fun v -> &a[t] ~~>[GMem] v), by := reduce_sum_empty(B)");
    for (int i = 0; i < N; i++) {
      __spreserves("&a[t] ~~>[GMem] reduce_sum(i, B)");
      __GHOST_BEGIN(focus, ro_matrix1_focus, "b, i");
      const int va = __gmem_get(&a[t]);
      const int vb = __gmem_get(&b[MINDEX1(N,i)]);
      __gmem_set(&a[t], va + vb);
      __GHOST_END(focus);
      __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
      __ghost(rewrite_linear, "inside := (fun v -> &a[t] ~~>[GMem] v), by := reduce_sum_add_right(i, B, i_geq_0)");
    }
  }
}

__device;
void read_thread_inner(int *a, int *b, int N) {
  __requires("B: int -> int");
  __requires("bpg: int, smem_sz: int");
  __reads("KernelParams(bpg, MSIZE1(N), smem_sz)");
  __preserves("ThreadsCtx(MINDEX1(0,0) ..+ MSIZE1(N))");
  __writes("desync_for i in ..N -> &a[MINDEX1(N,i)] ~~>[GMem] reduce_sum(N,B)");
  __reads("for i in 0..N -> &b[MINDEX1(N,i)] ~~>[GMem] B(i)");

  __threadfor; for (int t = 0; t < N; t++) {
    __xwrites("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(0, B)");
    __gmem_set(&a[MINDEX1(N,t)], 0);
    __ghost(rewrite_linear, "inside := (fun v -> &a[MINDEX1(N,t)] ~~>[GMem] v), by := reduce_sum_empty(B)");
  }

  for (int i = 0; i < N; i++) {
    __spreserves("desync_for t in ..N -> &a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(i, B)");
    __threadfor; for (int t = 0; t < N; t++) {
      __xconsumes("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(i, B)");
      __xproduces("&a[MINDEX1(N,t)] ~~>[GMem] reduce_sum(i+1, B)");
      __GHOST_BEGIN(focus, ro_matrix1_focus, "b, i");
      const int va = __gmem_get(&a[MINDEX1(N,t)]);
      const int vb = __gmem_get(&b[MINDEX1(N,i)]);
      __gmem_set(&a[MINDEX1(N,t)], va + vb);
      __GHOST_END(focus);
      __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
      __ghost(rewrite_linear, "inside := (fun v -> &a[MINDEX1(N,t)] ~~>[GMem] v), by := reduce_sum_add_right(i, B, i_geq_0)");
    }
  }
}
