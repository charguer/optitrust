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
  
We will start with a simple implementation of memset where matrix are only 1 dimensional

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

This allows us to get a **term** in the AST which represents the entry point of the for loop  

## 2. Checks
There are several things to check that the transformation can be applied : 
### 2.1 Instruction in the loop 
We want to apply it only when we have one for loop that contains exactly one instruction which has to a write operation on a flat one-dimensional array 

The loop term can be split into three parts 
- loop_range : record representing the loops iterators
- body : A new term representing the sequence of instructions inside the loop
- contracts : we leave it for now. 
Therefore we are intersted in getting the body part. To do so, we will use the functions :  `trm_inv` and `trm_for_inv` : trm_for_inv gives acces to the three subparts of the loop and trm_inv allows us to trigger a proper error if the term was not representing a loop. 
``` ocaml
let memset_apply ?(typ:typ option) (t : trm) :trm =
    let for_error = "Matrix_basic.memset_apply: expected for loop"  in
    let (range, body, contract) = trm_inv ~error:for_error trm_for_inv t in
```
The structure `trm_inv error trm_struct_inv ...` is really common and enable to properly access a subnode in the AST.
Therefore to continue the check we continue with the following lines :    
``` ocaml
let for_error = "Matrix_basic.memset_apply: expected for loop"  in
    let (range, body, contract) = trm_inv ~error:for_error trm_for_inv t in
    let seq_error = "Matrix_basic.memset_apply: expected exactly one instr in loop body" in
    let instr  = trm_inv ~error:seq_error trm_seq_single_inv body  in
    let aff_error = "Matrix_basic.memset_apply: expected a set operation" in
    let (lhs,rhs) = trm_inv ~error:aff_error trm_set_inv instr in
    let arr_error = "Matrix_basic.memset_apply: expected an array affectation" in
    let (array,dims,indices) = trm_inv ~error:arr_error Matrix_trm.access_inv lhs in
```
We get the (first and only one) instruction using `trm_seq_single_inv` instruction then the left part of the affectation 
