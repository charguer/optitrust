#include "optitrust_common.h"
#include "optitrust_intrinsics.h"
#include <optitrust_models.h>

__GHOST(array_extract) {
  // Assume start of 0
  __requires("i: int, len: int, items: int -> HProp");
  __requires("bounds_check: in_range(i, 0..len)");
  __consumes("for j in 0..len -> items(j)");
  // TODO: Could I have also done j <> i -> items(j) here?
  // I was getting an assert error:
  /*
    Resource computation error: File "lib/framework/resources/resource_computation.ml", line 355, characters 12-18: Assertion failed
  */
  // which I thought made sense because if it's wrong (e.g. i is outside the the range) it would not be using resources linearly.
  // So i switched it to this, but I got the same error in other parts?
  // Not sure what it is complaining about exactly?
  __produces("for j in 0..i -> items(j), for j in i+1..len -> items(j), items(i)");
  __admitted();
}

__GHOST(array_insert) {
  __reverts(array_extract);
  __admitted();
}

__DECL(countelems, "int * (int -> int) * int -> int");
__AXIOM(countelems_base, "forall (A: int -> int) (x: int) -> 0 = countelems(0, A, x)");
__AXIOM(countelems_ind_eq, "forall (n: int) (A: int -> int) (x: int) -> n >= 0 -> x = A(n) -> countelems(n, A, x) + 1 = countelems(n + 1, A, x)");
__AXIOM(countelems_ind_neq, "forall (n: int) (A: int -> int) (x: int) -> n >= 0 -> x <> A(n) -> countelems(n, A, x) = countelems(n + 1, A, x)");
__DEF(hist_of, "fun (A: int -> int) (arr_len: int) -> fun (j:int) -> countelems(arr_len, A, j)");

void hist(int *hist, int *arr, int hist_len, int arr_len) {
  __requires("A: int -> int");
  __requires("A_bounds: forall i in 0..arr_len -> in_range(A(i), 0..hist_len)");
  __reads("arr ~> Matrix1(arr_len, A)");
  __writes("hist ~> Matrix1(hist_len, hist_of(A, arr_len))");

  for (int j = 0; j < hist_len; j++) {
    __xwrites("&hist[MINDEX1(hist_len,j)] ~~> countelems(0, A, j)");
    hist[MINDEX1(hist_len,j)] = 0;
    __ghost(rewrite_linear, "inside := fun v -> &hist[MINDEX1(hist_len,j)] ~~> v, by := countelems_base(A,j)");
  }

  for (int i = 0; i < arr_len; i++) {
    __spreserves("hist ~> Matrix1(hist_len, hist_of(A,i))");
    __GHOST_BEGIN(focusA, ro_matrix1_focus, "arr, i");
    const int ind = arr[MINDEX1(arr_len,i)];
    __GHOST_END(focusA);
    __ghost(assert_prop, "in_range(i, 0..arr_len)", "i_in_range <- proof");
    __ghost(assert_prop, "in_range(ind, 0..hist_len), A_bounds(i, i_in_range)", "a_i_in_range <- proof");

    // Split array so we can update hist at this index
    // Focus won't work; we're not gonna give back that value, and we don't want the original array back anyway
    __ghost(array_extract, "i := ind, len := hist_len, items := fun i1 -> &hist[MINDEX1(hist_len, i1)] ~~> hist_of(A,i)(i1)");
    hist[MINDEX1(hist_len,ind)] += 1;

    // Prove that index we updated corresponds to i+1 step (countelems_ind_eq)
    __ghost(in_range_bounds, "i", "i_ge_0 <- lower_bound");
    __ghost(assume, "P := A(i) = A(i)", "ai_eq_refl <- H");
    __ghost(rewrite_linear, "inside := fun v -> &hist[MINDEX1(hist_len,A(i))] ~~> v, by := countelems_ind_eq(i, A, A(i))(i_ge_0)(ai_eq_refl)");

    // Prove that indices we *didnt* update corresponds to i+1 step (countelems_ind_neq)
#define HIST_SPLIT_PROOF\
      __xconsumes("&hist[MINDEX1(hist_len,j)] ~~> countelems(i,A,j)");\
      __xproduces("&hist[MINDEX1(hist_len,j)] ~~> countelems(i+1,A,j)"); \
      __ghost(assume, "P := j <> A(i)", "j_neq <- H"); \
      __ghost(rewrite_linear, "inside := fun v -> &hist[MINDEX1(hist_len,j)] ~~> v, by := countelems_ind_neq(i, A, j)(i_ge_0)(j_neq)");

    for (int j = 0; j < ind; j++) {
      HIST_SPLIT_PROOF
    }
    for (int j = ind+1; j < hist_len; j++) {
      HIST_SPLIT_PROOF
    }

    // Merge everything back together
    __ghost(array_insert, "i := ind, len := hist_len, items := fun i1 -> &hist[MINDEX1(hist_len, i1)] ~~> hist_of(A,i+1)(i1)");
  }

}

// A version of hist that adds to the hist array, just to see how the contract changes
/*
__DECL(pointwise_add, "(int -> int) * (int -> int) -> (int -> int)");

void hist_adds(int *hist, int *arr, int hist_len, int arr_len) {
  __requires("H: int -> int, A: int -> int");
  __requires("A_bounds: forall i in 0..arr_len -> in_range(A(i), 0..hist_len)");
  __reads("arr ~> Matrix1(arr_len, A)");
  __consumes("hist ~> Matrix1(hist_len, H)");
  __produces("hist ~> Matrix1(hist_len, hist_of(A) + H)");
}
  */

