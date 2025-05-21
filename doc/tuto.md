[WORK IN PROGRESS]


This document assumes a working installation of OptiTrust as described in `INSTALL.md`.


[TODO: rewrite array/matrix accesses using MINDEX macros]

# First transformations using OptiTrust

## Simple loop splitting 

Input

```c
const int N = 51200000;

// void stencil1D(double* a, double* b, int N) {
void stencil1D(double a[N], double b[N]) {
  for (int i = 1; i < N; i++) {
    // a[MINDEX1(N, i)]
    a[i] = (a[i] + a[i - 1]) / 2;
    b[i] = (b[i] + b[i - 1]) / 2;
  }
}
```

Output

```c
const int N = 51200000;

void stencil1D(double a[N], double b[N]) {
  for (int i = 1; i < N; i++) {
    a[i] = (a[i] + a[i - 1]) / 2;
  }
  for (int i = 1; i < N; i++) {
    b[i] = (b[i] + b[i - 1]) / 2;
  }
}
```

```ocaml
!! Loop.fission [cFor "i"; cWrite ~lhs:[cVar "a"] ()]


!! Loop.fission [cFor "i"; dNth 1]
```

benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp1_6_stencil1d.c



## Loop splitting with multiple loops

Input

```c
const int N = 2000;

void matmat_kernel(double C[N][N], double A[N][N], double B[N][N]) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      C[i][j] = 0.; // C[MINDEX2(N,N,i,j)]
      for (int k = 0; k < N; k++) {
        C[i][j] += A[i][k] * B[k][j];
      }
    }
  }
}
```

Output

```c
const int N = 2000;

void matmat_kernel(double C[N][N], double A[N][N], double B[N][N]) {

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      C[i][j] = 0.;
    }
  }
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {  
      for (int k = 0; k < N; k++) {
        C[i][j] += A[i][k] * B[k][j];
      }
    }
  }
}
```

variante: flatten the loops (for k... k <N*N) Loop.collapse
 and use memset to implement as a variant like intro of memcpy using stack_copy_on in matmul demo 

benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp2_1_matmat.c


## Reduction with reset

Input

```c
const int N = 5000;

void reduction_reinit(double A[N][N], double* sum) { 
  *sum = 0.;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      *sum += A[i][j];
      A[i][j] = 0.;
    }
  }
}
```

Output Version 1

```c
const int N = 5000;

void reduction_reinit(double A[N][N], double* sum) { 
  *sum = 0.;
  int sums[nbCores]; // init à zéro
  #pragma omp parallel 
  for (int i = 0; i < N; i++) {
    int s = 0;
    for (int j = 0; j < N; j++) {
      s += A[i][j];
    }
    sums[myThreadId()] = s
  }
  for idthreads
  *sum += sums[idthreads]

  #pragma omp parallel 
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      A[i][j] = 0.;
    }
  }
}
```

in pic_demo.ml
```
!! Matrix.delocalize "depositCorners" ~into:"depositThreadCorners" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var_mut "nbThreads") ~index:"idThread" ~acc_in_place:true ~ops:delocalize_sum ~use:(Some (var "idThread"))
```

Version 2: flatten the loops

Version 3: use a memset for the reset instead of the loop

benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp1_7_reductionreinit.c



## Skewing


Input

```c
const int N = 5000;

void polynomial_multiply(double c[2*N-1], double a[N], double b[N]) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      c[i+j] += a[i] * b[j];
    }
  }
}
```

Output 

```c
const int N = 5000;

void polynomial_multiply(double c[2*N-1], double a[N], double b[N]) {
  for (int k = 0; k < 2*N-1; k++) {
    for (int i = max(0,k-N+1); i < max(k+1,N); i++) {
      c[k] += a[i] * b[k-i];
    }
  }
}
```

Intermediate versions:

```c
{
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      int k = i + j;
      c[k] += a[i] * b[j];
    }
  }
}

// introduce a loop on k restricted to a single iteration
{
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      for (int k = 0; k < 2*N+1; k++) {
        if (k == i+j) {
          c[k] += a[i] * b[j];
        }
      }
    }
  }
}
// loop reorder, rewrite if-condition
{
  for (int k = 0; k < 2*N+1; k++) {
    for (int i = 0; i < N; i++) {
      for (int j = 0; j < N; j++) {
        if (j == k-i) {
          c[k] += a[i] * b[j];
        }
      }
    }
  }
}
// eliminate loop on j since it is a single iteration
{
  for (int k = 0; k < 2*N+1; k++) {
    for (int i = 0; i < N; i++) {
      int j = k-i;
      if (j >= 0 && j < N) {
        c[k] += a[i] * b[k-i];
      }
    }
  }
}
// work out what is needed to conclude.
{
  for (int k = 0; k < 2*N+1; k++) {
    for (int i = 0; i < N; i++) {
      if (k-i >= 0 && k-i < N) {
      // if (i <= k && i > k-N) {
      // if (i >= k-N+1 && i < k+1) {
        c[k-i] += a[i] * b[k-i];
      }
    }
  }
}
// work out what is needed to conclude.
{
  for (int k = 0; k < 2*N+1; k++) {
    for (int i = max(0,k-N+1); i < max(k,N); i++) {
      if (k-i >= 0 && k-i < N) {
        c[k-i] += a[i] * b[k-i];
      }
    }
  }
}
```

benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp1_8_polynomial.c

