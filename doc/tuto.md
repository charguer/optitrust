[WORK IN PROGRESS]


This document assumes a working installation of OptiTrust as described in `INSTALL.md`.


[TODO: rewrite array/matrix accesses using MINDEX macros]

# First transformations using OptiTrust

## Simple loop splitting 

Starting from the following C++ code, we aim to perform loop fission in order to parallelize each resulting loop independently.
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

Desired Output

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
### Creating the `stencil` Directory

We start by creating a new folder called `stencil`, and we add two files:

- `stencil1D.ml`: contains the transformation script.
- `stencil1D.cpp`: contains the original C++ code.

OptiTrust automatically associates the `.ml` file with the `.cpp` file of the same base name that it is supposed to transform.

### Running the Transformation

To have OptiTrust perform the transformation, we need to specify:

- Which transformation to apply,
- Where in the code to apply it.

In our case, we want to apply the following transformation:

```ocaml
Loop.fission [c1; ...; cn]
```
Here, [c1; ...; cn] is a list of constraints used to locate the corresponding AST node(s) in the C++ code.
There are several ways to describe the constraints leading to the fission. For example : 
```ocaml
Loop.fission [cForBody "i"; tBefore; dSeqNth 1]
```
which can be summed up as : "We want to locate a for loop over index i, and insert the fission right before the second statement inside the loop body" 
Finally, we have the following script : 
```ocaml
open Optitrust
open Prelude

let _ = Flags.check_validity := false

let _ = Run.script_cpp (fun _ ->
  !! Loop.fission [cForBody "i"; tBefore; dSeqNth 1];
)

```
Press F5 to see the diff on your browser. 
F6 if you want to look only at the diff for this precise transformation 
and shift + F5 is for the full trace
F5 : Repeat the last F5/F6 
F5 and F10 can be used anywhere. 
benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp1_6_stencil1d.c



## Loop splitting with multiple loops

### Matrix Multiplication Loop Fission

We now consider a matrix multiplication kernel. We want to apply a loop fission to transform the code as follows:

#### Input

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
#### Output
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
To achieve this, we apply a loop fission, specifying the point where the fission should occur.
We also need to indicate the loop nesting level to OptiTrust so that it targets the correct loops.

Here is the script:
```ocaml
open Optitrust
open Prelude

let _ = Flags.check_validity := false

let _ = Run.script_cpp (fun _ ->
  !! Loop.fission ~nest_of:2 [cForBody "i"; cWrite ~lhs:[cVar "C"] ~rhs:[cDouble 0.] (); tAfter]
)

```
Press F5 to see the diff in your browser
#### Extension 
We can also flatten the outerloop using 
```ocaml
 Loop.collapse [cFor "i"];
```
variante: flatten the loops (for k... k <N*N) Loop.collapse
 and use memset to implement as a variant like intro of memcpy using stack_copy_on in matmul demo 

benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp2_1_matmat.c


## Reduction with reset

Input

```c
const int N = 5000;
\
void reduction_reinit(double A[N][N], double* sum){
 *sum = 0
 double x;
 x = *sum;
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x += A[i][j];
      A[i][j] = 0.;
    }
  }
 *sum = x
}



void reduction_reinit(double A[N][N], double* sum) { 
 
 *sum = 0
 double x[1][n_cores];
 x[0] = 0; ??
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x[n_thread] += A[i][j];
      A[i][j] = 0.;
    }
x[myThreadId()] = s
  
  }
  for
  *sum = 0;
}
```

Output Version 1

```c
const int N = 5000;

void reduction_reinit(double A[N][N], double* sum) { 
  *sum = 0.;
  int sums[nbCores];
   // init à zéro
  #pragma omp parallel for

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


void reduction_reinit(double A[N][N], double* sum) {
 *sum = 0.;
 #pragma omp parallel
 for (int i = 0; i < N; i++) {
  for (int j = 0; j < N; j++) {
   *sum += A[i][j];
  }
 }
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
!! Matrix.delocalize "x" ~into:"x_of_threads" ~indices:["idCell"; "idCorner"]
      ~init_zero:true ~dim:(var_mut "nbThreads") ~index:"idThread" ~acc_in_place:true ~ops:delocalize_sum ~use:(Some (var "idThread"))
```
Delocalize doesn't work -> Variable delocalize is commented, pico_ml delocalize doesn't work because type has changed from string to var
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
Est ce que c est des transfos qu on veut faire a la main / par rapport a juste le rajouter dans le  
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
Ctrl + F10 run the test in the terminal 
F10 : repeat last Ctrl + F10
benchmarking code: http://icps.u-strasbg.fr/people/bastoul/public_html/teaching/parallelisme/codes/tp1_8_polynomial.c

