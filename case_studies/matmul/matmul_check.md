## 0. Naive code

```c
void mm(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");

  for (int i = 0; i < m; i++) {
    __modifies("Group(range(0, n, 1), fun j ->"
      " &C[MINDEX2(m, n, i, j)] ~> Cell)");
    __sequentially_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

    for (int j = 0; j < n; j++) {
      __modifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      __sequentially_reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __ghost(matrix2_ro_focus, "A, i, k");
        __ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        __ghost(matrix2_ro_unfocus, "A");
        __ghost(matrix2_ro_unfocus, "B");
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}

void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  mm(C, A, B, 1024, 1024, 1024);
}
```

## 1. Function.inline_def

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  for (int i = 0; i < 1024; i++) {
    __modifies("Group(range(0, 1024, 1), fun j ->"
      " &C[MINDEX2(1024, 1024, i, j)] ~> Cell)");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int j = 0; j < 1024; j++) {
      __modifies("&C[MINDEX2(1024, 1024, i, j)] ~> Cell");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      float sum = 0.0f;
      for (int k = 0; k < 1024; k++) {
        __ghost(matrix2_ro_focus, "A, i, k");
        __ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(1024, 1024, i, k)] * B[MINDEX2(1024, 1024, k, j)];
        __ghost(matrix2_ro_unfocus, "A");
        __ghost(matrix2_ro_unfocus, "B");
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
```

## 2. List.iter tile

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __modifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          float sum = 0.f;
          for (int bk = 0; bk < 256; bk++) {
            for (int k = 0; k < 4; k++) {
              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
          C[bi * 32 + i][bj * 32 + j] = sum;
        }
      }
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 3. Loop.reorder_at

## 3.1. Loop.hoist_alloc

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __modifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __modifies("&sum[j] ~> Cell");
          __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          sum[j] = 0.f;
          for (int bk = 0; bk < 256; bk++) {
            for (int k = 0; k < 4; k++) {
              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        free(sum);
      }
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.2. Loop.fission_all_instrs

- first change `__sequentially_reads` to `__reads`
- 'j' loop must be parallel

TODO: minimize contracts?

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __modifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        for (int j = 0; j < 32; j++) {
          // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __modifies("&sum[j] ~> Cell"); // write-only
          // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          sum[j] = 0.f;
        }
        for (int j = 0; j < 32; j++) {
          // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __modifies("&sum[j] ~> Cell");
          __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          for (int bk = 0; bk < 256; bk++) {
            for (int k = 0; k < 4; k++) {
              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell"); // write-only
          __modifies("&sum[j] ~> Cell"); // read-only
          // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        free(sum);
      }
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.3. Loop.swap

- 'j' loop must be parallel

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __modifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          __sequentially_reads("Group(range(j), A ~> Matrix2(1024, 1024)),"
                               "Group(range(j), B ~> Matrix2(1024, 1024))");

          for (int j = 0; j < 32; j++) {
            __modifies("&sum[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

            for (int k = 0; k < 4; k++) {
              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell"); // write-only
          __reads("&sum[j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        free(sum);
      }
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.4. Loop.swap

- 'j' loop must be parallel

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __modifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          __sequentially_reads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                               "Group(range(32), B ~> Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            __sequentially_reads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                                 "Group(range(32), B ~> Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __modifies("&sum[j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        free(sum);
      }
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.5. Loop.swap

- first loop.fission ghosts out of loops (similar to reorder_at implementation itself, recursive call?)
- 'i' loop must be parallel

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
    __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");

    for (int i = 0; i < 32; i++) {
      __consumes("Group(range(0, 1024, 1), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __produces("Group(range(0, 32, 1), fun bj -> Group(range(0, 32, 1), fun j ->"
                 "  &C[bi * 32 + i][bj * 32 + j] ~> Cell))");

      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    __ghost(group_swap, "to_item := fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell");

    for (int bj = 0; bj < 32; bj++) {
      __modifies("Group(range(i), fun i -> Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell))");
      __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
                           "B ~> Group(range(i), Matrix2(1024, 1024))");

      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
                               "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
                                 "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __modifies("&sum[j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        free(sum);
      }
    }

    __ghost(group_swap, "to_item := fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell");

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
    __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.6. Loop.hoist_alloc

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
    __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
      //                      "B ~> Group(range(i), Matrix2(1024, 1024))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");

        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
          //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              // __modifies("&sum[i][j] ~> Cell");
              // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }

        __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
        __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
      }
      free(sum);
    }

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    __ghost(ro_join_group, "H := B ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
    __ghost(ro_join_group, "H := A ~> Matrix2(1024, 1024), r := range(0, 32, 1)");
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.7. Loop.fission_all_instrs

- 'i' loop must be parallelizable

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
      //                      "B ~> Group(range(i), Matrix2(1024, 1024))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        for (int bk = 0; bk < 256; bk++) {
          // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
          //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              // __modifies("&sum[i][j] ~> Cell");
              // __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 3.8. Loop.swap

- 'i' loop must be parallelizable

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
      //                      "B ~> Group(range(i), Matrix2(1024, 1024))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024)),"
        //                      "B ~> Group(range(32), fun i -> Matrix2(1024, 1024))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              // __modifies("&sum[i][j] ~> Cell");
              // __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 4. Loop.hoist_expr

- need to justify correctness of `Loop.move_out`s

## 4.1. Loop.hoist_alloc

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  for (int bi = 0; bi < 32; bi++) {
    // __sequentially_modifies("Group(range(32), fun bj -> Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))))");
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
      //                      "B ~> Group(range(i), Matrix2(1024, 1024))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024)),"
        //                      "B ~> Group(range(32), fun i -> Matrix2(1024, 1024))");

        for (int i = 0; i < 32; i++) {
          // __sequentially_modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)));
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          for (int k = 0; k < 4; k++) {
            __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
            // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __modifies("&pB[bj][bk][k][j] ~> Cell");
              __modifies("&sum[i][j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              pB[bj][bk][k][j] = B[bk * 4 + k][bj * 32 + j];
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
              __ghost(matrix2_ro_unfocus, "A");
              __ghost(matrix2_ro_unfocus, "B");
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

## 4.2. Loop.hoist_instr

- need to minimize loop contracts
- need to move around read_seq/reads and focus/unfocus ghosts
- FIXME: previous 'j', 'k', 'bk', 'bj' loops must be parallelizable

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  // TODO: ghost seq_reads to reads
  for (int bj = 0; bj < 32; bj++) {
    __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __reads("B ~> Matrix2(1024, 1024))");

    for (int bk = 0; bk < 256; bk++) {
      __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sequentially_reads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sequentially_reads("B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __modifies("&pB[bj][bk][k][j] ~> Cell");
          __sequentially_reads("B ~> Matrix2(1024, 1024))");

          __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          pB[bj][bk][k][j] = B[bk * 4 + k][bj * 32 + j];
          __ghost(matrix2_ro_unfocus, "B");
        }
      }
    }
  }
  // TODO: ghost reads to seq_reads
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024))");
      __reads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024))");
        __reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024)");
          __sequentially_reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024))");
            __reads("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");

            for (int j = 0; j < 32; j++) {
              __modifies("&sum[i][j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024)");
              __reads("&pB[bj][bk][k][j] ~> Cell");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
              __ghost(matrix2_ro_unfocus, "A");
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 5. Matrix.stack_copy

- TODO: could be a combination of heap copy + moving to stack
- need memcpy contract

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  // TODO: ghost seq_reads to reads
  for (int bj = 0; bj < 32; bj++) {
    __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __reads("B ~> Matrix2(1024, 1024))");

    for (int bk = 0; bk < 256; bk++) {
      __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sequentially_reads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sequentially_reads("B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __modifies("&pB[bj][bk][k][j] ~> Cell");
          __sequentially_reads("B ~> Matrix2(1024, 1024))");

          __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          pB[bj][bk][k][j] = B[bk * 4 + k][bj * 32 + j];
          __ghost(matrix2_ro_unfocus, "B");
        }
      }
    }
  }
  // TODO: ghost reads to seq_reads
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024))");
      __reads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024))");
        __reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024)");
          __sequentially_reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

          float s[32];
          // TODO: wrap in ghosts going to byte model? how to enforce contiguity?
          memcpy(s, &sum[i][0], sizeof(float[32]));
          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &s[j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024))");
            __reads("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");

            for (int j = 0; j < 32; j++) {
              __modifies("&s[j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024)");
              __reads("&pB[bj][bk][k][j] ~> Cell");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              s[j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
              __ghost(matrix2_ro_unfocus, "A");
            }
          }
          // TODO: wrap in ghosts going to byte model? how to enforce contiguity?
          memcpy(&sum[i][0], s, sizeof(float[32]));
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 6. Matrix.elim_mops

- need ghosts to see matrix as flat array

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  // TODO: ghost seq_reads to reads
  for (int bj = 0; bj < 32; bj++) {
    __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __reads("B ~> Matrix2(1024, 1024))");

    for (int bk = 0; bk < 256; bk++) {
      __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sequentially_reads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sequentially_reads("B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __modifies("&pB[bj][bk][k][j] ~> Cell");
          __sequentially_reads("B ~> Matrix2(1024, 1024))");

          __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          // TODO: need ghosts to see matrix as flat array
          pB[32768 * bj + 128 * bk + 32 * k + j] = B[1024 * (4 * bk + k) + bj * 32 + j];
          __ghost(matrix2_ro_unfocus, "B");
        }
      }
    }
  }
  // TODO: ghost reads to seq_reads
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024))");
      __reads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          sum[32 * i + j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024))");
        __reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024)");
          __sequentially_reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

          float s[32];
          // TODO: ghosts?
          memcpy(s, &sum[32 * i], sizeof(float[32]));
          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &s[j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024))");
            __reads("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");

            for (int j = 0; j < 32; j++) {
              __modifies("&s[j] ~> Cell");
              __reads("A ~> Matrix2(1024, 1024)");
              __reads("&pB[bj][bk][k][j] ~> Cell");

              __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              // TODO: need ghosts to see matrix as flat array
              s[j] += A[1024 * (bi * 32 + i) + bk * 4 + k] *
                      pB[32768 * bj + 128 * bk + 32 * k + j];
              __ghost(matrix2_ro_unfocus, "A");
            }
          }
          // TODO: ghosts?
          memcpy(&sum[32 * i], s, sizeof(float[32]));
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          C[1024 * (bi * 32 + i) + bj * 32 + j] = sum[32 * i + j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 7. Loop.unroll

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  // TODO: ghost seq_reads to reads
  for (int bj = 0; bj < 32; bj++) {
    __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __reads("B ~> Matrix2(1024, 1024))");

    for (int bk = 0; bk < 256; bk++) {
      __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sequentially_reads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sequentially_reads("B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __modifies("&pB[bj][bk][k][j] ~> Cell");
          __sequentially_reads("B ~> Matrix2(1024, 1024))");

          __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          // TODO: need ghosts to see matrix as flat array
          pB[32768 * bj + 128 * bk + 32 * k + j] = B[1024 * (4 * bk + k) + bj * 32 + j];
          __ghost(matrix2_ro_unfocus, "B");
        }
      }
    }
  }
  // TODO: ghost reads to seq_reads
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sequentially_reads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024))");
      __reads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          sum[32 * i + j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024))");
        __reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024)");
          __sequentially_reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

          float s[32];
          // TODO: ghosts?
          memcpy(s, &sum[32 * i], sizeof(float[32]));
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][0][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 0");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 0] *
                    pB[32768 * bj + 128 * bk + 32 * 0 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][1][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 1");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 1] *
                    pB[32768 * bj + 128 * bk + 32 * 1 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][2][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 2");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 2] *
                    pB[32768 * bj + 128 * bk + 32 * 2 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][3][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 3");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 3] *
                    pB[32768 * bj + 128 * bk + 32 * 3 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          // TODO: ghosts?
          memcpy(&sum[32 * i], s, sizeof(float[32]));
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          C[1024 * (bi * 32 + i) + bj * 32 + j] = sum[32 * i + j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```

# 8. Omp.simd + 9. Omp.parallel_for

- inner accumulating 'j' loops need to be parallelizable
- outer 'bj' and 'bi' loops need to be parallelizable

```c
void mm1024(float* C, float* A, float* B) {
  __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");
  __modifies("C ~> Matrix2(1024, 1024)");

  __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");

  float* const pB = (float* const)malloc(sizeof(float[32][256][4][32]));
  // TODO: ghost seq_reads to reads
  #pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    __modifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __reads("B ~> Matrix2(1024, 1024))");

    for (int bk = 0; bk < 256; bk++) {
      __modifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sequentially_reads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __modifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sequentially_reads("B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __modifies("&pB[bj][bk][k][j] ~> Cell");
          __sequentially_reads("B ~> Matrix2(1024, 1024))");

          __ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          // TODO: need ghosts to see matrix as flat array
          pB[32768 * bj + 128 * bk + 32 * k + j] = B[1024 * (4 * bk + k) + bj * 32 + j];
          __ghost(matrix2_ro_unfocus, "B");
        }
      }
    }
  }
  // TODO: ghost reads to seq_reads
  // TODO: ghost seq_reads to reads
  #pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    __modifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __reads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    for (int i = 0; i < 32; i++) {
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024))");
      __reads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");

      float* const sum = (float* const)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        // TODO: ghost seq_reads to reads (maybe also fissioned from here??)
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          sum[32 * i + j] = 0.f;
        }
      }
      for (int bk = 0; bk < 256; bk++) {
        // __sequentially_modifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        // __sequentially_reads("A ~> Group(range(32), fun i -> Matrix2(1024, 1024))");
        __reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __modifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __reads("A ~> Matrix2(1024, 1024)");
          __sequentially_reads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

          float s[32];
          // TODO: ghosts?
          memcpy(s, &sum[32 * i], sizeof(float[32]));
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][0][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 0");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 0] *
                    pB[32768 * bj + 128 * bk + 32 * 0 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][1][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 1");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 1] *
                    pB[32768 * bj + 128 * bk + 32 * 1 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][2][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 2");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 2] *
                    pB[32768 * bj + 128 * bk + 32 * 2 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            __modifies("&s[j] ~> Cell");
            __reads("A ~> Matrix2(1024, 1024)");
            __reads("&pB[bj][bk][3][j] ~> Cell");

            __ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 3");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 3] *
                    pB[32768 * bj + 128 * bk + 32 * 3 + j];
            __ghost(matrix2_ro_unfocus, "A");
          }
          // TODO: ghosts?
          memcpy(&sum[32 * i], s, sizeof(float[32]));
        }
      }
      for (int i = 0; i < 32; i++) {
        __reads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __reads("&sum[i][j] ~> Cell");

          // TODO: need ghosts to see matrix as flat array
          C[1024 * (bi * 32 + i) + bj * 32 + j] = sum[32 * i + j];
        }
        // TODO: ghost reads to seq_reads (maybe also fissioned from here??)
      }
      free(sum);
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) {
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
  }
  // TODO: ghost reads to seq_reads
  free(pB);
  __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
}
```
