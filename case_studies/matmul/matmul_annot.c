/*
# Raw AST -> AST

Reconnaitre appels à
__requires("")
__ensures("")

for(int i = a; i < b; ++i) {
    __invariant("")
    __iter_local_invariant("")
}

__ghost("");



## Avec sucre
__reads("P") = __requires("'a: frac; P/'a") __ensures("P/'a")
__modifies("P") = __requires("P"); __ensures("P")

Dépend de la sytaxe des ressources
__consumes("P") = __requires("P")
__produces("P") = __ensures("P")


## Syntaxe de requires/ensures
void concat(int* a, int* b) {
    __requires("f: frac;"
               "b: ptr<int>;"
               "a => Array;"
               "b =>^{f} Array<Integer>")
}*/


void matrix_copy(float* in, float* out, int n, int m) {
    __reads("in => Matrix");
    __modifies("out => Matrix");
    __requires("Hsz_in: dim($in) = (n, m); Hsz_out: dim($out) = (n, m)");
    __ensures("$out = $in");
}

pour matrix_inplace_add:
__ensures("$out = Matrix.add($in, \old($out))");

M => MatrixSized<R>(n, m)
    <=>
M => Matrix<R>; Hsz: MatrixSize(M, (n, m))
    ou Hsz: dim($M) = (n, m)

MatrixSize(M, (n, m)); M => Matrix<R> ==> dim($M) = (n, m)


#include <stdlib.h>

#include "omp.h"

void mm(float* C, float* A, float* B, int m, int n, int p) {
  __reads("a => MatrixSized<float>(m, o); b => MatrixSized(o, n)");
  __modifies("output => MatrixSized(m, n)");
  for (int i = 0; i < m; i++) {
    __reads("a; b");
    __modifies("output");
    for (int j = 0; j < n; j++) {
      float sum = 0.0f;
      // alloc(sum)
      for (int k = 0; k < p; k++) {
        __reads("a; b")
        __modifies("sum => Cell<float>")
        sum += A[MINDEX2(m, p, i, k)] * B[MINDEX2(p, n, k, j)];
      }

      C[MINDEX2(m, n, i, j)] = sum;

      // free(sum)
    }
  }
}

// = *(i in [0; m[, j in [0; n[) output[i][j] -> cell float
ghost matrix_open(ptr<float> out, int m, int n) {
__consumes("out => MatrixSize(m,n)");
__produces("GroupRanges([(0,m); (0,n)],\
            fun (i, j) -> output[MINDEX(m, n, i, j)] => Cell)");


__produces("GroupRange(0, m, fun i ->
            GroupRange(0, n, fun j ->
            output[MINDEX(m, n, i, j)] => Cell))");
*/

int find_one_divisor(int n) {
    ensures("H: not_prime n")
    produces("r: int; P: r * $res = n")

    ...
}

void test_divisor() {
    int a = find_divisor(34);
    __binds("d := r; Hd := P");
    __assert("d * a = 34")
}

T* malloc2(int m, int n, size_t sz) {
    __produces("res => MatrixSized<T>(m, n)");
}

__ghost("MatrixTile.tile_dim1_relative(bt, sz)");
// consumes(bt => MatrixTile((x, m), (y, n)))
// requires(divides sz m)
// produces(t: Group(range(0, m, sz), fun bj' ->
//             bt => MatrixTile((x+bj', sz), (y, n))))
//     OU
__ghost("MatrixTile.tile_dim1_absolute(bt, sz)");
// produces(t: Group(range(x, x+m, sz), fun bj' ->
//             bt => MatrixTile((bj', sz), (y, n))))

void mm(float* output, float* a, float* b, int m, int n, int o) {
  __reads("a => MatrixSized(m, o); b => MatrixSized(o, n)");
  __reads("a => Matrix; a :> MatrixSize(m, 0); b => MatrixSized(o, n)");
  __modifies("output => MatrixSized(m, n)");

  float* bt = malloc2(n, o, sizeof(float));
  // bt -> matrix (n, o) float
  {
    __ghost("MatrixTile.intro(bt)");
    // consumes(bt => Matrix); reads(bt :> MatrixSize(n, o))
    // ALT: consumes(bt => MatrixSized(n, o))
    // produces(bt => MatrixTile((0, n), (0, o)))
    __ghost("MatrixTile.tile_dim1_relative(bt, 32)");
    // consumes(bt => MatrixTile((0, n), (0, o)))
    // requires(divides 32 n)
    // produces(t: Group(range(0, n, 32), fun bj' ->
    //             bt => MatrixTile((bj', 32), (0, o))))

    // assert(Group(range(0, n, 32), fun bj ->
    //        bt => MatrixTile((bj, 32), (0, o))))
#pragma omp parallel for
    for (int bj = 0; bj < n; bj += 32) {
      __isolatedly_modifies("bt => MatrixTile((bj, 32), (0, o))")
      __ghost("MatrixTile.tile_dim2_relative(bt, 1)");
      // produces(t: Group(range(0, o), fun k ->
      //             bt => MatrixTile((bj, 32), (k, 1))))
      for (int k = 0; k < o; k++) {
        __isolatedly_modifies("bt => MatrixTile((bj, 32), (k, 1))")
        __ghost("MatrixTile.tile_dim1_relative(bt, 1)");
      // produces(t: Group(range(0, o), fun k ->
      //          bt => MatrixTile((bj, 32), (k, 1))))

#pragma omp simd
        for (int j = 0; j < 32; j++) {
          __isolatedly_modifies("bt => MatrixTile((bj + j, 1), (k, 1))")
          // bt[j + bj][k] -> cell float
          // =
          // bt -> matrix_tile ((bj+j, 1), (0, o)) (n, o) float
          bt[MINDEX2(n, o, j + bj, k)] = b[MINDEX2(o, n, j + bj, k)];
          // Check à l'écriture:
          // Vérifier qu'on est dans la tuile
          // read bt :> MatrixSize(n, o)
        }
      }
    }
  }
  {
#pragma omp parallel for
    for (int bi = 0; bi < m; bi += 32) {
      for (int bj = 0; bj < n; bj += 32) {
        // output -> matrix_tile ((bi, 32), (bj, 32)) (m, n) float
        // = *(i in [bi; bi+32[, j in [bj; bj+32[) output[i][j] -> cell float
        float sum[32][32];
        // sum -> matrix (32, 32) float
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            // sum[i][j] -> cell float
            sum[i][j] = 0.;
          }
        }
        for (int bk = 0; bk < o; bk += 4) {
          for (int i = 0; i < 32; i++) {
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              // sum[i][j] -> cell float
              sum[i][j] += a[0 + bk + o * (i + bi)] * bt[j + bj][0 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[1 + bk + o * (i + bi)] * bt[j + bj][1 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[2 + bk + o * (i + bi)] * bt[j + bj][2 + bk];
            }
#pragma omp simd
            for (int j = 0; j < 32; j++) {
              sum[i][j] += a[3 + bk + o * (i + bi)] * bt[j + bj][3 + bk];
            }
          }
        }
        for (int i = 0; i < 32; i++) {
#pragma omp simd
          for (int j = 0; j < 32; j++) {
            // output[j + bj + n * (i + bi)] -> cell float
            // =
            // output[i + bi][j + bj] -> cell float
            // =
            // output -> matrix_tile ((bi, 32), (bj+j, 1)) (m, n) float
            output[j + bj + n * (i + bi)] = sum[i][j];
          }
        }
      }
    }
  }
}

int main() {
  const int M = 1024;
  const int N = 1024;
  const int O = 1024;
  float* output = (float*)calloc(M * N, sizeof(float));
  float* a = (float*)malloc(M * O * sizeof(float));
  float* b = (float*)malloc(O * N * sizeof(float));
  mm(output, a, b, M, N, O);
  free(output);
  free(a);
  free(b);
  return 0;
}
