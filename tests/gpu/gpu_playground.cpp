#include <optitrust_gpu.h>

void test(int *a, int M, int N) {
  __requires("t: int, ");
  //__preserves("ThreadsCtx( MINDEX1(0,0) ..+ MSIZE1(N) )");
  __writes("a ~> Matrix2(M, N, fun (i j: int) -> i + j)");

  for (int i = 0; i < M; i++) {
    __xwrites("for t in 0..N -> &a[MINDEX2(M,N,i,t)] ~~> i + t");
    for (int t = 0; t < N; t++) {
      __xwrites("&a[MINDEX2(M,N,i,t)] ~~> i + t");
      a[MINDEX2(M,N,i,t)] = i + t;
    }
//    magic_barrier();
  }
}
