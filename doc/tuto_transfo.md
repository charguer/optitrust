# Create our first transfo with Optitrust
As seen in the first tutorial, Optitrust relies on transformations in order to ...
The user can define his own transformations, this is the topic of this tutorial. 
In the introduction tutorial, we have transformed the code : 

```c
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
into : 
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
But we can notice thqt it would be more efficient to directly set the C's values to 0 instead of doing it manually. Therefore we willl create a transformation `Matrix_basic.memset` that will produce this transformation. 

The transformation plan is as follows:

1. **Identify the target**  
   Define a set of constraints that allows OptiTrust to locate the AST node where the transformation should be applied.

2. **Checks**  
  Ensure that the specified path correctly identifies the intended subnodes in the AST.

3. **Construct the replacement**  
   Define the new AST terms that should replace the original ones.

## 1. Identify the target 
Here it's the most simple part : the only constraint needed is a for loop whose index is "i" : 
Therefore the transformation will look like : 
`Matrix_basic.memset [ cFor "i"]`
In the `lib.transfo.Matrix_basic.ml` we will define a new transformation : 

```ocaml 
let%transfo memset (tg:target) : unit =
  apply_at_target_paths memset_apply tg;;
```
A transformation is defined using let%transfo and, in our case of one line, `apply_at_target_paths memset_apply tg;;` 

## 2. Checks
There are several things to check that the transformation can be applied : 
-> We want to apply it only when we have one for loop that contains exactly one instruction which is an array set 
   
   let trm_fors_depth t : trm : int =
  let rec aux acc t = match trm_for_inv
