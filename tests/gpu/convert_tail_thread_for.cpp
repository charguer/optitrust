#include <optitrust_gpu.h>

void single(int *a, int *b, int N) {
  __requires("B: int -> int");
  __reads("b ~> Matrix1(N, B)");
  __writes("a ~> Matrix1(N, B)");
  for (int i = 0; i < N; i++) {
    __sreads("b ~> Matrix1(N, B)");
    __xwrites("&a[MINDEX1(N, i)] ~~> B(i)");
    __GHOST_BEGIN(focusb, ro_matrix1_focus, "b, i");
    a[MINDEX1(N, i)] = b[MINDEX1(N, i)];
    __GHOST_END(focusb);
  }
}

void nested(int *a, int *b, int N, int M) {
  __requires("B: int -> int");
  __reads("b ~> Matrix1(M, B)");
  __writes("a ~> Matrix2(N, M, fun (i: int) (j: int) -> B(j))");
  for (int i = 0; i < N; i++) {
    __sreads("b ~> Matrix1(M, B)");
    __xwrites("for j in 0..M -> &a[MINDEX2(N, M, i, j)] ~~> B(j)");
    for (int j = 0; j < M; j++) {
      __sreads("b ~> Matrix1(M, B)");
      __xwrites("&a[MINDEX2(N, M, i, j)] ~~> B(j)");
      __GHOST_BEGIN(focusb, ro_matrix1_focus, "b, j");
      a[MINDEX2(N, M, i, j)] = b[MINDEX1(M, j)];
      __GHOST_END(focusb);
    }
  }
}

void nested_seq(int *a, int *b, int N, int K, int M) {
  __requires("B: int -> int");
  __reads("b ~> Matrix1(M, B)");
  __writes("a ~> Matrix3(N, K, M, fun (i: int) (k: int) (j: int) -> B(j))");
  for (int i = 0; i < N; i++) {
    __sreads("b ~> Matrix1(M, B)");
    __xwrites("for k in 0..K -> for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
    for (int k = 0; k < K; k++) {
      __sreads("b ~> Matrix1(M, B)");
      __xwrites("for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
      for (int j = 0; j < M; j++) {
        __sreads("b ~> Matrix1(M, B)");
        __xwrites("&a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
        __GHOST_BEGIN(focusb, ro_matrix1_focus, "b, j");
        a[MINDEX3(N, K, M, i, k, j)] = b[MINDEX1(M, j)];
        __GHOST_END(focusb);
      }
    }
  }
}
