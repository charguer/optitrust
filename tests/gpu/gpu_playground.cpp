#include <optitrust_gpu.h>

void test(int *a, int M, int N) {
  __requires("t: int, tpb: int");
  //__preserves("ThreadsCtx( MINDEX1(0,0) ..+ MSIZE1(N) )");
  __writes("a ~> Matrix2(M, N, fun (i j: int) -> i + j)");
  __preserves("KernelSetupCtx");
  __preserves("KernelTeardownCtx");
  __preserves("ThreadsCtx(t ..+ MSIZE0())");
  __reads("KernelParams(MSIZE3(3,4,5), tpb, sizeof(int) * 6 + 0)");
  __preserves("SMemAllowance(sizeof(int) * 6 + 0)");


  for (int i = 0; i < M; i++) {
    __xwrites("for t in 0..N -> &a[MINDEX2(M,N,i,t)] ~~> i + t");
    for (int t = 0; t < N; t++) {
      __xwrites("&a[MINDEX2(M,N,i,t)] ~~> i + t");
      a[MINDEX2(M,N,i,t)] = i + t;
    }
//    magic_barrier();
  }

  {
    int* const a = TREG_REF_S(int, 0);
    int b = __treg_get(a) + 1;
    __treg_set(a, 1);
  }

  /*int * const b = MALLOC4(int, 3,4,5,6);
  magic_barrier();
  free(b);*/
}


/*void test2() {
  for (int k = 0; k < 10; k++) {
    float * const arr = MALLOC1(float, 69);
    free(arr);
  }
}*/
