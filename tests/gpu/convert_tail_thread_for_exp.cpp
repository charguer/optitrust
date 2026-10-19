#include <optitrust_gpu.h>

void single(int* a, int* b, int N) {
  __requires("B: int -> int");
  __writes("a ~> Matrix1(N, B)");
  __reads("b ~> Matrix1(N, B)");
  __ghost(ro_fork_group, "f := #_1, H := b ~> Matrix1(N, B), r := 0..N");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("_RO(#_1 / range_count(0..N), b ~> Matrix1(N, B))");
    __xproduces("_RO(#_1 / range_count(0..N), Desync(b ~> Matrix1(N, B)))");
    __xwrites("&a[MINDEX1(N, i)] ~~> B(i)");
    const __ghost_fn focusb =
        __ghost_begin(ro_matrix1_focus, "matrix := b, i := i");
    a[MINDEX1(N, i)] = b[MINDEX1(N, i)];
    __ghost_end(focusb);
    __ghost(ro_desync_intro,
            "f := #_1 / range_count(0..N), H := b ~> Matrix1(N, B)");
  }
  __ghost(ro_desync_join_desyncgroup,
          "f := #_1, H := b ~> Matrix1(N, B), n := N");
  magic_barrier();
}

void nested(int* a, int* b, int N, int M) {
  __requires("B: int -> int");
  __writes("a ~> Matrix2(N, M, fun (i: int) (j: int) -> B(j))");
  __reads("b ~> Matrix1(M, B)");
  __ghost(ro_fork_group, "f := #_1, H := b ~> Matrix1(M, B), r := 0..N");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("_RO(#_1 / range_count(0..N), b ~> Matrix1(M, B))");
    __xconsumes("for j in 0..M -> &a[MINDEX2(N, M, i, j)] ~> UninitCell");
    __xproduces("_RO(#_1 / range_count(0..N), Desync(b ~> Matrix1(M, B)))");
    __xproduces("desync_for j in ..M -> &a[MINDEX2(N, M, i, j)] ~~> B(j)");
    __ghost(ro_fork_group,
            "f := #_1 / range_count(0..N), H := b ~> Matrix1(M, B), r := 0..M");
    __magic_threadfor;
    for (int j = 0; j < M; j++) {
      __strict();
      __xconsumes(
          "_RO(#_1 / range_count(0..N) / range_count(0..M), b ~> Matrix1(M, "
          "B))");
      __xproduces(
          "_RO(#_1 / range_count(0..N) / range_count(0..M), Desync(b ~> "
          "Matrix1(M, B)))");
      __xwrites("&a[MINDEX2(N, M, i, j)] ~~> B(j)");
      const __ghost_fn focusb =
          __ghost_begin(ro_matrix1_focus, "matrix := b, i := j");
      a[MINDEX2(N, M, i, j)] = b[MINDEX1(M, j)];
      __ghost_end(focusb);
      __ghost(ro_desync_intro,
              "f := #_1 / range_count(0..N) / range_count(0..M), H := b ~> "
              "Matrix1(M, B)");
    }
    __ghost(ro_desync_join_desyncgroup,
            "f := #_1 / range_count(0..N), H := b ~> Matrix1(M, B), n := M");
  }
  __ghost(ro_desync_join_desyncgroup,
          "f := #_1, H := b ~> Matrix1(M, B), n := N");
  magic_barrier();
}

void nested_seq(int* a, int* b, int N, int K, int M) {
  __requires("B: int -> int");
  __writes("a ~> Matrix3(N, K, M, fun (i: int) (k: int) (j: int) -> B(j))");
  __reads("b ~> Matrix1(M, B)");
  __ghost(ro_fork_group, "f := #_1, H := b ~> Matrix1(M, B), r := 0..N");
  __magic_threadfor;
  for (int i = 0; i < N; i++) {
    __strict();
    __xconsumes("_RO(#_1 / range_count(0..N), b ~> Matrix1(M, B))");
    __xconsumes(
        "for k in 0..K -> for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> "
        "UninitCell");
    __xproduces("_RO(#_1 / range_count(0..N), Desync(b ~> Matrix1(M, B)))");
    __xproduces(
        "for k in 0..K -> desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] "
        "~~> B(j)");
    __ghost(ro_fork_group,
            "f := #_1 / range_count(0..N), H := b ~> Matrix1(M, B), r := 0..K");
    for (int k = 0; k < K; k++) {
      __strict();
      __xconsumes(
          "_RO(#_1 / range_count(0..N) / range_count(0..K), b ~> Matrix1(M, "
          "B))");
      __xconsumes(
          "for j in 0..M -> &a[MINDEX3(N, K, M, i, k, j)] ~> UninitCell");
      __xproduces(
          "_RO(#_1 / range_count(0..N) / range_count(0..K), Desync(b ~> "
          "Matrix1(M, B)))");
      __xproduces(
          "desync_for j in ..M -> &a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
      __ghost(ro_fork_group,
              "f := #_1 / range_count(0..N) / range_count(0..K), H := b ~> "
              "Matrix1(M, B), r := 0..M");
      __magic_threadfor;
      for (int j = 0; j < M; j++) {
        __strict();
        __xconsumes(
            "_RO(#_1 / range_count(0..N) / range_count(0..K) / "
            "range_count(0..M), b ~> Matrix1(M, B))");
        __xproduces(
            "_RO(#_1 / range_count(0..N) / range_count(0..K) / "
            "range_count(0..M), Desync(b ~> Matrix1(M, B)))");
        __xwrites("&a[MINDEX3(N, K, M, i, k, j)] ~~> B(j)");
        const __ghost_fn focusb =
            __ghost_begin(ro_matrix1_focus, "matrix := b, i := j");
        a[MINDEX3(N, K, M, i, k, j)] = b[MINDEX1(M, j)];
        __ghost_end(focusb);
        __ghost(ro_desync_intro,
                "f := #_1 / range_count(0..N) / range_count(0..K) / "
                "range_count(0..M), H := b ~> Matrix1(M, B)");
      }
      __ghost(ro_desync_join_desyncgroup,
              "f := #_1 / range_count(0..N) / range_count(0..K), H := b ~> "
              "Matrix1(M, B), n := M");
    }
    __ghost(ro_desync_join_group,
            "f := #_1 / range_count(0..N), H := b ~> Matrix1(M, B), n := K");
  }
  __ghost(ro_desync_join_desyncgroup,
          "f := #_1, H := b ~> Matrix1(M, B), n := N");
  magic_barrier();
}
