#include <optitrust_gpu.h>

void single(int* a, int* b, int N) {
  __requires("B: int -> int");
  __writes("a ~> Matrix1(N, B)");
  __reads("b ~> Matrix1(N, B)");
  __magic_threadfor_one;
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
  __ghost(dro_fork_group, "H := b ~> Matrix1(M, B), r := 0..N");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("for j in 0..M -> &a[MINDEX2(N, M, i, j)] ~> UninitCell");
    __xproduces("desync_for j in ..M -> &a[MINDEX2(N, M, i, j)] ~~> B(j)");
    __xreads("b ~> Matrix1(M, B)");
    __magic_threadfor_one;
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
  __ghost(ro_join_group, "H := b ~> Matrix1(M, B), r := 0..N");
}

void nested_seq(int* a, int* b, int N, int K, int M) {
  __requires("B: int -> int");
  __writes("a ~> Matrix3(N, K, M, fun (i: int) (k: int) (j: int) -> B(j))");
  __reads("b ~> Matrix1(M, B)");
  __ghost(dro_fork_group, "H := b ~> Matrix1(M, B), r := 0..N");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __requires("#_2: _Fraction");
    __xconsumes("Desync_RO(#_2, b ~> Matrix1(M, B))");
    __xconsumes(
        "for k in 0..K -> for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> "
        "UninitCell");
    __xproduces(
        "for k in 0..K -> Desync_RO(#_2 / range_count(0..K), b ~> Matrix1(M, "
        "B))");
    __xproduces(
        "for k in 0..K -> desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] "
        "~~> B(j)");
    __ghost(dro_fork_group, "H := b ~> Matrix1(M, B), r := 0..K");
    for (int k = 0; k < K; k++) {
      __strict();
      __requires("#_3: _Fraction");
      __xconsumes(
          "for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> UninitCell");
      __xproduces(
          "desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
      __xpreserves("Desync_RO(#_3, b ~> Matrix1(M, B))");
      __magic_threadfor_one;
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
  for (int i = 0; i < N; i++) {
    __strict();
    __requires("#_2: _Fraction");
    __xconsumes(
        "_RO(#_2 / range_count(0..K), for k in 0..K -> b ~> Matrix1(M, B))");
    __xproduces("_RO(#_2, b ~> Matrix1(M, B))");
    __ghost(ro_join_group, "H := b ~> Matrix1(M, B), r := 0..K");
  }
  __ghost(ro_join_group, "H := b ~> Matrix1(M, B), r := 0..N");
}
