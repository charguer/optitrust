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
        // TODO: ghost seq_reads to reads
        for (int j = 0; j < 32; j++) {
          // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __modifies("&sum[j] ~> Cell"); // write-only
          // __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          sum[j] = 0.f;
        }
        for (int j = 0; j < 32; j++) {
          // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __modifies("&sum[j] ~> Cell");
          __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

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
          // __sequentially_reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        // TODO: ghost reads to seq_reads
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
        // TODO: ghost seq_reads to reads
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          // __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
          //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int j = 0; j < 32; j++) {
            // __modifies("&sum[j] ~> Cell");
            // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

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
        // TODO: ghost reads to seq_reads
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
        // TODO: ghost seq_reads to reads
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          // __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
          //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              // __modifies("&sum[j] ~> Cell");
              // __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

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
        // TODO: ghost reads to seq_reads
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

    for (int i = 0; i < 32; i++) { // maybe also fissioned above??
      __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }

    // TODO: ghost seq_reads to reads
    for (int bj = 0; bj < 32; bj++) {
      // __sequentially_modifies("Group(range(i), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
      // __sequentially_reads("A ~> Group(range(i), Matrix2(1024, 1024)),"
      //                      "B ~> Group(range(i), Matrix2(1024, 1024))");

      for (int i = 0; i < 32; i++) {
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float* const)malloc(sizeof(float[32]));
        // TODO: ghost seq_reads to reads
        for (int j = 0; j < 32; j++) {
          __modifies("&sum[j] ~> Cell");

          sum[j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          // __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
          //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            // __sequentially_modifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            // __sequentially_reads("A ~> Group(range(j), Matrix2(1024, 1024)),"
            //                      "B ~> Group(range(j), Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              // __modifies("&sum[j] ~> Cell");
              // __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

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
        // TODO: ghost reads to seq_reads
        free(sum);
      }
    }
    // TODO: ghost reads to seq_reads

    for (int i = 0; i < 32; i++) { // maybe also fissioned below??
      __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
    }
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
        __modifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        // TODO: ghost seq_reads to reads
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
              // __/*sequentially_*/reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

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
        // TODO: ghost reads to seq_reads
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
