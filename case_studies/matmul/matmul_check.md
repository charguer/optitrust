## 0. Naive code

```c
void mm(float* C, float* A, float* B, int m, int n, int p) {
  __reads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");
  __modifies("C ~> Matrix2(m, n)");

  for (int i = 0; i < m; i++) {
    __xmodifies("for j in 0..n ->"
      " &C[MINDEX2(m, n, i, j)] ~> Cell");
    __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

    for (int j = 0; j < n; j++) {
      __xmodifies("&C[MINDEX2(m, n, i, j)] ~> Cell");
      __sreads("A ~> Matrix2(m, p), B ~> Matrix2(p, n)");

      float sum = 0.0f;
      for (int k = 0; k < p; k++) {
        __scoped_ghost(matrix2_ro_focus, "A, i, k");
        __scoped_ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
        // __ghost(matrix2_ro_unfocus, "A");
        // __ghost(matrix2_ro_unfocus, "B");
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
    __xmodifies("for j in 0..1024 ->"
      " &C[MINDEX2(1024, 1024, i, j)] ~> Cell");
    __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int j = 0; j < 1024; j++) {
      __xmodifies("&C[MINDEX2(1024, 1024, i, j)] ~> Cell");
      __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      float sum = 0.0f;
      for (int k = 0; k < 1024; k++) {
        __scoped_ghost(matrix2_ro_focus, "A, i, k");
        __scoped_ghost(matrix2_ro_focus, "B, k, j");
        sum += A[MINDEX2(1024, 1024, i, k)] * B[MINDEX2(1024, 1024, k, j)];
      }

      C[MINDEX2(m, n, i, j)] = sum;
    }
  }
}
```

## 2. List.iter tile

```c
[...]
  __scoped_ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)");
  for (int bi = 0; bi < 32; bi++) {
    __xmodifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    for (int i = 0; i < 32; i++) {
      __xmodifies("Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell)");
      __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

      __scoped_ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      for (int bj = 0; bj < 32; bj++) {
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          float sum = 0.f;
          for (int bk = 0; bk < 256; bk++) {
            for (int k = 0; k < 4; k++) {
              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
          C[bi * 32 + i][bj * 32 + j] = sum;
        }
      }
      /* __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
        " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)"); */
    }
  }
  /* __ghost(untile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
    " to_item := fun i -> Group(range(1024), fun j -> &C[i][j] ~> Cell)"); */
[...]
```

# 3. Loop.reorder_at

## 3.1. Loop.hoist_alloc

```c
[...]
        float* const sum = (float*)malloc(sizeof(float[32]));
        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xmodifies("&sum[j] ~> Cell");
          __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          sum[j] = 0.f;
          for (int bk = 0; bk < 256; bk++) {
            for (int k = 0; k < 4; k++) {
              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
          C[bi * 32 + i][bj * 32 + j] = sum[j];
        }
        free(sum);
[...]
```

## 3.2. Loop.fission_all_instrs

- first change `__sequentially_reads` to `__reads`
- 'j' loop must be parallel

TODO: minimize contracts?

```c
[...]
        float* const sum = (float*)malloc(sizeof(float[32]));
        { // not a seq
          __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
          __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
          for (int j = 0; j < 32; j++) {
            // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
            __xmodifies("&sum[j] ~> Cell"); // write-only
            // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

            sum[j] = 0.f;
          }
          for (int j = 0; j < 32; j++) {
            // __modifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
            __xmodifies("&sum[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

            for (int bk = 0; bk < 256; bk++) {
              for (int k = 0; k < 4; k++) {
                __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
                __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
                sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              }
            }
          }
          for (int j = 0; j < 32; j++) {
            __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell"); // write-only
            __xmodifies("&sum[j] ~> Cell"); // read-only
            // __reads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

            C[bi * 32 + i][bj * 32 + j] = sum[j];
          }
        }
        free(sum);
[...]
```

## 3.3. Loop.swap

- 'j' loop must be parallel

```c
[...]
        for (int bk = 0; bk < 256; bk++) {
          __smodifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                               "Group(range(32), B ~> Matrix2(1024, 1024))");

          for (int j = 0; j < 32; j++) {
            __xmodifies("&sum[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

            for (int k = 0; k < 4; k++) {
              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
        }
[...]
```

## 3.4. Loop.swap

- 'j' loop must be parallel

```c
[...]
        for (int bk = 0; bk < 256; bk++) {
          __smodifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
          __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                               "Group(range(32), B ~> Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            __smodifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                                 "Group(range(32), B ~> Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __xmodifies("&sum[j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
        }
[...]
```

## 3.5. Loop.swap

- first loop.fission ghosts out of loops (similar to reorder_at implementation itself, recursive call?)
- 'i' loop must be parallel

```c
[...]
    __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
    __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
    __scoped_ghost {
      for (int i = 0; i < 32; i++) {
        __xconsumes("for j in 0..1024 ->  &C[bi * 32 + i][j] ~> Cell");
        __xproduces("for bj in 0..32 -> for j in 0..32 ->"
                  "  &C[bi * 32 + i][bj * 32 + j] ~> Cell");

        __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
          " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      }
    }

    __scoped_ghost(group_swap, "to_item := fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell");

    for (int bj = 0; bj < 32; bj++) {
      __xmodifies("Group(range(32), fun i -> Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell))");
      __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                           "Group(range(32), B ~> Matrix2(1024, 1024))");

      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        float* const sum = (float*)malloc(sizeof(float[32]));
        { // not a seq
          __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
          __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
          for (int j = 0; j < 32; j++) {
            __xmodifies("&sum[j] ~> Cell");

            sum[j] = 0.f;
          }
          for (int bk = 0; bk < 256; bk++) {
            __smodifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
            __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                                "Group(range(32), B ~> Matrix2(1024, 1024))");

            for (int k = 0; k < 4; k++) {
              __smodifies("Group(range(32), fun j -> &sum[j] ~> Cell)");
              __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                                  "Group(range(32), B ~> Matrix2(1024, 1024))");

              for (int j = 0; j < 32; j++) {
                __xmodifies("&sum[j] ~> Cell");
                __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

                __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
                __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
                sum[j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
              }
            }
          }
          for (int j = 0; j < 32; j++) {
            __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
            __xreads("&sum[j] ~> Cell");

            C[bi * 32 + i][bj * 32 + j] = sum[j];
          }
        }
        free(sum);
      }
    }
[...]
```

## 3.6. Loop.hoist_alloc

```c
[...]
      float* const sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");
        __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
        __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        for (int j = 0; j < 32; j++) {
          __xmodifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
        for (int bk = 0; bk < 256; bk++) {
          __smodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                               "Group(range(32), B ~> Matrix2(1024, 1024))");

          for (int k = 0; k < 4; k++) {
            __smodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                                "Group(range(32), B ~> Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __xmodifies("&sum[i][j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
        }
        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xreads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
      }
      free(sum);
[...]
```

## 3.7. Loop.fission_all_instrs

- 'i' loop must be parallelizable

```c
[...]
      float* const sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }
      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

        __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
        __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        for (int bk = 0; bk < 256; bk++) {
          [...]

          for (int k = 0; k < 4; k++) {
            [...]

            for (int j = 0; j < 32; j++) {
              __xmodifies("&sum[i][j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __xreads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xreads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
      }
      free(sum);
[...]
```

## 3.8. Loop.swap

- hot loop 'i' loop must be parallelizable

```c
[...]
      __scoped_ghost {
        for (int i = 0; i < 32; i++) {
          [...]

          __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
          __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        }
      }

      for (int bk = 0; bk < 256; bk++) {
        __smodifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        __sreads("Group(range(32), fun i -> A ~> Matrix2(1024, 1024)),"
                             "Group(range(32), fun i -> B ~> Matrix2(1024, 1024))");

        for (int i = 0; i < 32; i++) {
          __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          for (int k = 0; k < 4; k++) {
            [...]

            for (int j = 0; j < 32; j++) {
              __xmodifies("&sum[i][j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * B[bk * 4 + k][bj * 32 + j];
            }
          }
        }
      }
[...]
```

# 4. Loop.hoist_expr

- need to justify correctness of `Loop.move_out`s

## 4.1. Loop.hoist_alloc

```c
[...]
  float* const pB = (float*)malloc(sizeof(float[32][256][4][32]));
  for (int bi = 0; bi < 32; bi++) {
    __smodifies("Group(range(32), fun bj -> Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))))");
    __xmodifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

    __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
    __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
    __scoped_ghost {
      for (int i = 0; i < 32; i++) {
        __xconsumes("for j in 0..1024 -> &C[bi * 32 + i][j] ~> Cell");
        __xproduces("for bj in 0..32 -> for j in 0..32 ->"
                  "  &C[bi * 32 + i][bj * 32 + j] ~> Cell");

        __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
          " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      }
    }
    __scoped_ghost(group_swap, "to_item := fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell");

    for (int bj = 0; bj < 32; bj++) {
      __xmodifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
      __xmodifies("Group(range(32), fun i -> Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell))");
      __sreads("Group(range(32), A ~> Matrix2(1024, 1024)),"
                           "Group(range(32), B ~> Matrix2(1024, 1024))");

      float* const sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }

      __scoped_ghost {
        for (int i = 0; i < 32; i++) {
          [...]

          __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
          __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        }
      }

      // __ghost swap???

      for (int bk = 0; bk < 256; bk++) {
        __xmodifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
        [...]

        for (int i = 0; i < 32; i++) {
          __smodifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
          __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

          for (int k = 0; k < 4; k++) {
            __xmodifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
            [...]

            for (int j = 0; j < 32; j++) {
              __xmodifies("&pB[bj][bk][k][j] ~> Cell");
              __xmodifies("&sum[i][j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024), B ~> Matrix2(1024, 1024)");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
              pB[bj][bk][k][j] = B[bk * 4 + k][bj * 32 + j];
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __xreads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xreads("&sum[i][j] ~> Cell");

          C[bi * 32 + i][bj * 32 + j] = sum[i][j];
        }
      }
      free(sum);
    }
  }
  free(pB);
[...]
```

## 4.2. Loop.hoist_instr

- need to minimize loop contracts
- need to move around read_seq/reads and focus/unfocus ghosts
- FIXME: previous 'j', 'k', 'bk', 'bj' loops must be parallelizable (not really: write-only followed by read-only)

```c
// [...]
  for (int bj = 0; bj < 32; bj++) {
    __xmodifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __sreads("B ~> Matrix2(1024, 1024))");

    /* FIXME: what is happening here?
    __scoped_ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32"); // fait pour 'i'

    __scoped_ghost {
      for (int i = 0; i < 32; i++) {
          [...]

          __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        }
      }
    */

    for (int bk = 0; bk < 256; bk++) {
      __xmodifies("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
      __sreads("B ~> Matrix2(1024, 1024))");

      for (int k = 0; k < 4; k++) {
        __xmodifies("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
        __sreads("Group(range(32), B ~> Matrix2(1024, 1024))");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&pB[bj][bk][k][j] ~> Cell");
          __xreads("B ~> Matrix2(1024, 1024))");

          __scoped_ghost(matrix2_ro_focus, "B, bk * 4 + k, bj * 32 + j");
          pB[bj][bk][k][j] = B[bk * 4 + k][bj * 32 + j];
        }
      }
    }
  }
  for (int bi = 0; bi < 32; bi++) {
    __xmodifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __sreads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    __scoped_ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
    __scoped_ghost {
      for (int i = 0; i < 32; i++) {
        __xconsumes("for j in 0..1024 -> &C[bi * 32 + i][j] ~> Cell");
        __xproduces("for bj in 0..32 -> for j in 0..32 ->"
                  "  &C[bi * 32 + i][bj * 32 + j] ~> Cell");

        __ghost(tile_divides, "tile_count := 32, tile_size := 32, n := 1024,"
          " to_item := fun j -> &C[bi * 32 + i][j] ~> Cell)");
      }
    }
    __scoped_ghost(group_swap, "to_item := fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell");

    for (int bj = 0; bj < 32; bj++) {
      __xreads("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
      __xmodifies("Group(range(32), fun i -> Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell))");
      __sreads("Group(range(32), A ~> Matrix2(1024, 1024))");

      float* const sum = (float*)malloc(sizeof(float[32][32]));
      for (int i = 0; i < 32; i++) {
        __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&sum[i][j] ~> Cell");

          sum[i][j] = 0.f;
        }
      }

      __scoped_ghost {
        for (int i = 0; i < 32; i++) {
          // [...]

          __ghost(ro_fork_group, "H := A ~> Matrix2(1024, 1024), r := 0..32");
          // might be two for loops
          // __ghost(ro_fork_group, "H := B ~> Matrix2(1024, 1024), r := 0..32");
        }
      }

      for (int bk = 0; bk < 256; bk++) {
        __smodifies("Group(range(32), fun i -> Group(range(32), fun j -> &sum[i][j] ~> Cell))");
        __sreads("Group(range(32), fun i -> A ~> Matrix2(1024, 1024))");
        __xreads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");

        for (int i = 0; i < 32; i++) {
          __sreads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
          __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __xreads("A ~> Matrix2(1024, 1024)");

          for (int k = 0; k < 4; k++) {
            __xreads("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
            __smodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
            __sreads("Group(range(32), A ~> Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __xmodifies("&sum[i][j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024)");
              __xreads("&pB[bj][bk][k][j] ~> Cell");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              sum[i][j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
            }
          }
        }
      }
      for (int i = 0; i < 32; i++) {
        __xreads("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
        __xmodifies("Group(range(32), fun j -> &C[bi * 32 + i][bj * 32 + j] ~> Cell)");

        for (int j = 0; j < 32; j++) {
          __xmodifies("&C[MINDEX2(1024, 1024, bi * 32 + i, bj * 32 + j)] ~> Cell");
          __xreads("&sum[i][j] ~> Cell");

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
[...]
        for (int i = 0; i < 32; i++) {
          __sreads("Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell))");
          __xmodifies("Group(range(32), fun j -> &sum[i][j] ~> Cell)");
          __xreads("A ~> Matrix2(1024, 1024)");

          float s[32];
          // TODO: wrap in ghosts going to byte model? how to enforce contiguity?
          memcpy(s, &sum[i][0], sizeof(float[32]));
          for (int k = 0; k < 4; k++) {
            __xreads("Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)");
            __smodifies("Group(range(32), fun j -> &s[j] ~> Cell)");
            __sreads("Group(range(32), A ~> Matrix2(1024, 1024))");

            for (int j = 0; j < 32; j++) {
              __xmodifies("&s[j] ~> Cell");
              __xreads("A ~> Matrix2(1024, 1024)");
              __xreads("&pB[bj][bk][k][j] ~> Cell");

              __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + k");
              s[j] += A[bi * 32 + i][bk * 4 + k] * pB[bj][bk][k][j];
            }
          }
          // TODO: wrap in ghosts going to byte model? how to enforce contiguity?
          memcpy(&sum[i][0], s, sizeof(float[32]));
        }
[...]
```

# 6. Matrix.elim_mops

- need ghosts to see matrix as flat array

```c
[...]
          // TODO: need ghosts to see matrix as flat array
          pB[32768 * bj + 128 * bk + 32 * k + j] = B[1024 * (4 * bk + k) + bj * 32 + j];
[...]
[...]
          // TODO: need ghosts to see matrix as flat array
          sum[32 * i + j] = 0.f;
[...]
          float s[32];
          // TODO: ghosts?
          memcpy(s, &sum[32 * i], sizeof(float[32]));
            [...]
              // TODO: need ghosts to see matrix as flat array
              s[j] += A[1024 * (bi * 32 + i) + bk * 4 + k] *
                      pB[32768 * bj + 128 * bk + 32 * k + j];
          // TODO: ghosts?
          memcpy(&sum[32 * i], s, sizeof(float[32]));
[...]
          // TODO: need ghosts to see matrix as flat array
          C[1024 * (bi * 32 + i) + bj * 32 + j] = sum[32 * i + j];
[...]
```

# 7. Loop.unroll

```c
[...]
          for (int j = 0; j < 32; j++) {
            __xmodifies("&s[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024)");
            __xreads("&pB[bj][bk][0][j] ~> Cell");

            __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 0");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 0] *
                    pB[32768 * bj + 128 * bk + 32 * 0 + j];
          }
          for (int j = 0; j < 32; j++) {
            __xmodifies("&s[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024)");
            __xreads("&pB[bj][bk][1][j] ~> Cell");

            __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 1");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 1] *
                    pB[32768 * bj + 128 * bk + 32 * 1 + j];
          }
          for (int j = 0; j < 32; j++) {
            __xmodifies("&s[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024)");
            __xreads("&pB[bj][bk][2][j] ~> Cell");

            __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 2");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 2] *
                    pB[32768 * bj + 128 * bk + 32 * 2 + j];
          }
          for (int j = 0; j < 32; j++) {
            __xmodifies("&s[j] ~> Cell");
            __xreads("A ~> Matrix2(1024, 1024)");
            __xreads("&pB[bj][bk][3][j] ~> Cell");

            __scoped_ghost(matrix2_ro_focus, "A, bi * 32 + i, bk * 4 + 3");
            // TODO: need ghosts to see matrix as flat array
            s[j] += A[1024 * (bi * 32 + i) + bk * 4 + 3] *
                    pB[32768 * bj + 128 * bk + 32 * 3 + j];
          }
[...]
```

# 8. Omp.simd + 9. Omp.parallel_for

- inner accumulating 'j' loops need to be parallelizable
- outer 'bj' and 'bi' loops need to be parallelizable

```c
[...]
  // TODO: scoped ghost seq_reads to reads
  #pragma omp parallel for
  for (int bj = 0; bj < 32; bj++) {
    __xmodifies("Group(range(256), fun bk -> Group(range(4), fun k -> Group(range(32), fun j -> &pB[bj][bk][k][j] ~> Cell)))");
    __xreads("B ~> Matrix2(1024, 1024))");

    [...]
  }
  // TODO: scoped ghost seq_reads to reads
  #pragma omp parallel for
  for (int bi = 0; bi < 32; bi++) {
    __xmodifies("Group(range(32), fun i -> Group(range(1024), fun j -> &C[bi * 32 + i][j] ~> Cell))");
    __xreads("A ~> Matrix2(1024, 1024), pB ~> Matrix4(32, 256, 4, 32)");

    [...]
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            [...]
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            [...]
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            [...]
          }
          #pragma omp simd
          for (int j = 0; j < 32; j++) {
            [...]
          }
    [...]
  }
[...]
```
