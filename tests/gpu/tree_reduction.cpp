#include <optitrust_gpu.h>

/*
void ff (int y, int *a) {
  __consumes("If(y = 0, a ~~> 1)");
  __produces("If(y = 0, a ~~> 1 + 1)");
  if (y == 0) {
    __ghost(thing2);
    *a = *a + 1;
    __ghost(thing_rev);
  } else {
    __ghost(thing, "H2 := a ~~> 1 + 1");
  }
}
 */


__DECL(reduce_sum, "int * (int -> float) -> float");
__AXIOM(reduce_sum_empty, "forall (f: int -> float) -> 0.f =. reduce_sum(0, f)");
__AXIOM(reduce_sum_add_right, "forall (n: int) (f: int -> float) (_: n >= 0) -> reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)");

__DECL(tree_sum, "(int -> float) * int * int * int -> float");
__AXIOM(tree_sum_ind, "forall (f: int -> float) (n: int) (i: int) (t: int) (_: t < (1 << i)) -> "
  "tree_sum(f,n,i,t) =. tree_sum(f,n,i + 1,t) +. tree_sum(f,n,i + 1,t + (1 << i))");
__AXIOM(tree_sum_complete, "forall (f: int -> float) (n: int) -> tree_sum(f,n,0,0) =. reduce_sum(1 << n, f)");
__AXIOM(tree_sum_base, "forall (f: int -> float) (n: int) (t: int) -> f(t) =. tree_sum(f,n,n,t)");

// A group with nothing in it is equivalent to the "emp" permission
__GHOST(empty_group_of_anything) {
  __requires("H: int -> HProp, n: int");
  __produces("Group(n..n,H)");
  __admitted();
}
__GHOST(delete_empty_group_of_anything) {
  __reverts(empty_group_of_anything);
  __admitted();
}

float reduce(float *arr, int logN) {
  __requires("A: int -> float");
  __consumes("arr ~> Matrix1((1 << logN), A)");
  __ensures("_Res =. reduce_sum((1 << logN), A)");

  const int N = 1 << logN;

  float sum = 0.f;
  for (int t = 0; t < N; t++) {
    __xconsumes("&arr[MINDEX1(N,t)] ~~> A(t)");
    __xproduces("&arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,logN,t)");
    __ghost(rewrite_float_linear, "inside := (fun v -> &arr[MINDEX1(N,t)] ~~> v), by := tree_sum_base(A, logN, t)");
  }

  __admitted();

  __ghost(assume, "logN >= 0");
  for (int i = logN; i > 0; i--) {
    __spreserves("for i in 0..(1<<i) -> &arr[MINDEX1(N,t)] ~~> ");
    __spreserves("&sum ~~> reduce_sum(i,A)");
    __xreads("&arr[MINDEX1(N,i)] ~~> A(i)");

    sum += arr[MINDEX1(N,i)];
    __ghost(in_range_bounds, "i", "i_geq_0 <- lower_bound");
    __ghost(rewrite_float_linear, "inside := (fun v -> &sum ~~> v), by := reduce_sum_add_right(i, A, i_geq_0)");
  }

  __ghost(assume, "P := reduce_sum(N, A) =. reduce_sum(N, A)");
  return sum;
}
