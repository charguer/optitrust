#include "optitrust_models.h"
#include "optitrust_gpu.h"


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
    __xconsumes("DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0)");
    __xproduces("DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1)");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __GMEM_SET(&a[MINDEX2(N,M,i,j)],1);
    }
  }
  blocksync(); __with("H := DesyncGroup(r1, N, fun i -> DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1))");
}

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
    __xconsumes("DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0)");
    __xproduces("DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1)");
    __threadfor; for (int j = 0; j < M; j++) {
      __xconsumes("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
      __GMEM_SET(&a[MINDEX2(N,M,i,j)], 1);
    }
  }

  __ghost(eq_refl, "x := MSIZE2(N,M)");
  __ghost(desyncgroup_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1, size := MSIZE2(N,M), tile_count := N, tile_size := M");
  __ghost(rewrite_linear, "from := MSIZE2(N,M), to := MSIZE2(N*M/32, 32), inside := fun (sz: int) -> DesyncGroup(r1(sz), MSIZE2(N,M), fun j -> &a[j] ~~>[GMem] 1)");
  __ghost(rewrite_linear, "from := MSIZE2(N,M), to := MSIZE2(N*M/32, 32), inside := fun (sz: int) -> ThreadsCtx(range_plus(MINDEX1(sz,0), sz))");
  __ghost(chunk_range_plus2, "D2 := N*M/32, D1 := 32");
  __ghost(desyncgroup_tile_divides, "items := fun i -> &a[i] ~~>[GMem] 1, tile_count := N*M/32, tile_size := 32");

  __threadfor; for (int i = 0; i < N*M/32; i++) {
    __xconsumes("DesyncGroup(r3(i), 32, fun j -> &a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1)");
    __xproduces("DesyncGroup(r3(i), 32, fun j -> &a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1+1)");
    __threadfor; for (int j = 0; j < 32; j++) {
      __xconsumes("&a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1");
      __xproduces("&a[MINDEX2(N*M/32,32,i,j)] ~~>[GMem] 1+1");
      const int v = __GMEM_GET(&a[MINDEX2(N*M/32,32,i,j)]);
      __GMEM_SET(&a[MINDEX2(N*M/32,32,i,j)], v + 1);
    }
  }

  __ghost(eq_refl, "x := MSIZE2(N*M/32,32)");
  __ghost(desyncgroup_untile_divides, "items := fun k -> &a[k] ~~>[GMem] 1+1, size := MSIZE2(N*M/32,32), tile_count := N*M/32, tile_size := 32");
  __PROOF_OF(eq_retile_sym, "MSIZE2(N*M/32, 32) = MSIZE2(N,M)", "eq_sym(MSIZE2(N,M), MSIZE2(N*M/32, 32), eq_retile)");
  __ghost(rewrite_linear, "from := MSIZE2(N*M/32, 32), to := MSIZE2(N,M), inside := fun (sz: int) -> DesyncGroup(r1(sz), MSIZE2(N*M/32,32), fun j -> &a[j] ~~>[GMem] 1+1)");
  __ghost(rewrite_linear, "from := MSIZE2(N*M/32, 32), to := MSIZE2(N,M), inside := fun (sz: int) -> ThreadsCtx(range_plus(MINDEX1(sz,0), sz))");
  __ghost(chunk_range_plus2, "D2 := N, D1 := M");
  __ghost(desyncgroup_tile_divides, "items := fun i -> &a[i] ~~>[GMem] 1+1, tile_count := N, tile_size := M");

  blocksync(); __with("H := DesyncGroup(r1(MSIZE2(N,M)), N, fun i -> DesyncGroup(r2(i), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1+1))");
}

