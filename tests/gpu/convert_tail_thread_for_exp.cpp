#include <optitrust_gpu.h>

void single(int* a, int* b, int N) {
  __requires("B: int -> int");
  __writes("a ~> Matrix1(N, B)");
  __reads("b ~> Matrix1(N, B)");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __sreads("b ~> Matrix1(N, B)");
    __xwrites("&a[MINDEX1(N, i)] ~~> B(i)");
    const __ghost_fn focusb =
        __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
    a[MINDEX1(N, i)] = b[MINDEX1(N, i)];
    __ghost_end(focusb);
  }
  magic_barrier();
}

void nested(int* a, int* b, int N, int M) {
  __requires("B: int -> int");
  __writes("a ~> Matrix2(N, M, fun (i: int) (j: int) -> B(j))");
  __reads("b ~> Matrix1(M, B)");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __sreads("b ~> Matrix1(M, B)");
    __xconsumes("for j in 0..M -> &a[MINDEX2(N, M, i, j)] ~> UninitCell");
    __xproduces("desync_for j in ..M -> &a[MINDEX2(N, M, i, j)] ~~> B(j)");
    __magic_threadfor;
    for (int j = 0; j < M; j++) {
      __strict();
      __sreads("b ~> Matrix1(M, B)");
      __xwrites("&a[MINDEX2(N, M, i, j)] ~~> B(j)");
      const __ghost_fn focusb =
          __ghost_begin(ro_matrix1_focus, "matrix := b, i := j");
      a[MINDEX2(N, M, i, j)] = b[MINDEX1(M, j)];
      __ghost_end(focusb);
    }
  }
  magic_barrier();
}

void nested_seq(int* a, int* b, int N, int K, int M) {
  __requires("B: int -> int");
  __writes("a ~> Matrix3(N, K, M, fun (i: int) (k: int) (j: int) -> B(j))");
  __reads("b ~> Matrix1(M, B)");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __sreads("b ~> Matrix1(M, B)");
    __xconsumes(
        "for k in 0..K -> for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> "
        "UninitCell");
    __xproduces(
        "for k in 0..K -> desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] "
        "~~> B(j)");
    for (int k = 0; k < K; k++) {
      __strict();
      __sreads("b ~> Matrix1(M, B)");
      __xconsumes(
          "for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> UninitCell");
      __xproduces(
          "desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
      __magic_threadfor;
      for (int j = 0; j < M; j++) {
        __strict();
        __sreads("b ~> Matrix1(M, B)");
        __xwrites("&a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
        const __ghost_fn focusb =
            __ghost_begin(ro_matrix1_focus, "matrix := b, i := j");
        a[MINDEX3(N, K, M, i, k, j)] = b[MINDEX1(M, j)];
        __ghost_end(focusb);
      }
    }
  }
  magic_barrier();
}
