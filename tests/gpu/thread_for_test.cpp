#include "optitrust_models.h"
#include "optitrust_gpu.h"

__GHOST(group_to_desyncgroup1) {
  __requires("D1: int, items: int -> HProp");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE1(D1),0), MSIZE1(D1)))");
  __consumes("for i in 0..D1 -> items(i)");
  __produces("DesyncGroup(range_plus(MINDEX1(MSIZE1(D1),0), MSIZE1(D1)), D1, fun i -> items(i))");
  __admitted();
}

__GHOST(group_to_desyncgroup2) {
  __requires("D1: int, D2: int, items: int*int -> HProp");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)))");
  __consumes("for i in 0..D2 -> for j in 0..D1 -> items(i,j)");
  __produces("DesyncGroup(range_plus(MINDEX1(MSIZE2(D2,D1),0), MSIZE2(D2,D1)), D2, fun i -> DesyncGroup(range_plus(MINDEX2(D2,MSIZE1(D1),i,0), MSIZE1(D1)), D1, fun j -> items(i,j) ) )");
  __admitted();
}

__DECL(ok_mem, "MemType -> Prop");
__AXIOM(gmem_ok_mem, "ok_mem(GMem)");

void sync_test() {
  __requires("H: HProp, r: Range");
  __preserves("ThreadsCtx(r)");
  __consumes("H");
  __produces("Sync(ok_mem, H)");
  __admitted();
}

void stuff(int *a, int N, int M) {
  /*__writes("a ~~> 1");
  *a = 1;
  *a = 2;*/
  __requires("N: int, M: int, A: int * int -> int");
  __preserves("ThreadsCtx(range_plus(MINDEX1(MSIZE2(N,M),0), MSIZE2(N,M)))");
  __consumes("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  __produces("for i in 0..N -> for j in 0..M -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 1");
  /*__produces("DesyncGroup(range_plus(MINDEX1(MSIZE2(N,M),0), MSIZE2(N,M)), N, fun i -> DesyncGroup(range_plus(MINDEX2(N,MSIZE1(M),i,0), MSIZE1(M)), M, fun j -> &a[MINDEX2(N,M,i,j)] ~~> 1))");*/
  __ghost(group_to_desyncgroup2, "items := fun i j -> &a[MINDEX2(N,M,i,j)] ~~>[GMem] 0");
  // clang_to_ast.ml
  // arthur: concrete syntax
  // Trm_for should have an extra argument
  // Smart constructor for trm_for should have a default
  // actual Trm_for does not appear so often
  __threadfor; for (int j = 0; j < N; j++) {
    __xconsumes("DesyncGroup(range_plus(MINDEX2(N,MSIZE1(M),j,0), MSIZE1(M)), M, fun k -> &a[MINDEX2(N,M,j,k)] ~~>[GMem] 0)");
    __xproduces("DesyncGroup(range_plus(MINDEX2(N,MSIZE1(M),j,0), MSIZE1(M)), M, fun k -> &a[MINDEX2(N,M,j,k)] ~~>[GMem] 1)");
    __threadfor; for (int k = 0; k < M; k++) {
      __xconsumes("&a[MINDEX2(N,M,j,k)] ~~>[GMem] 0");
      __xproduces("&a[MINDEX2(N,M,j,k)] ~~>[GMem] 1");
      __GMEM_SET(&a[MINDEX2(N,M,j,k)],1);
    }
  }
  sync_test();
}

/*
void stuff2(int *a, int N, int M) {
  __requires("N: int, M: int, A: int * int -> int");
  __consumes("for i in 0..N -> for j in 0..M+i -> &a[MINDEX2(N,M,i,j)] ~~> 0");
  __produces("for i in 0..N -> for j in 0..M+i -> &a[MINDEX2(N,M,i,j)] ~~> 1");
  __DEF(Arange, "fun (i: int) -> 0..M+i");
  __DEF(Acell, "fun (v: int) (i1: int) (i2: int) -> &a[MINDEX2(N,M,i1,i2)] ~~> v");
    __DEF(Agroup, "fun (v: int) (i1: int) -> Group(r, fun i2 -> Acell(v, i1, i2))");
  for (int i1 = 0; i1 < N; i1++) {
    //__xrequires("r: Range");
    __xconsumes("Group(Arange(i1), fun i2 -> &a[MINDEX2(N,M,i1,i2)] ~~> 0)");
    __xproduces("Group(Arange(i1), fun i2 -> &a[MINDEX2(N,M,i1,i2)] ~~> 1)");
  // __xconsumes("Agroup(0, )");
  // __xproduces("Agroup(1)");
    //__xconsumes("Group(r, fun i2 -> Acell(1, i2))");
    for (int i2 = 0; i2 < M+i1; i2++) {
      __xconsumes("&a[MINDEX2(N,M,i1,i2)] ~~> 0");
      __xproduces("&a[MINDEX2(N,M,i1,i2)] ~~> 1");
      a[MINDEX2(N,M,i1,i2)] = 1;
    }
  }
}
*/
