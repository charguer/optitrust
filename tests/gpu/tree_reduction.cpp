#include "optitrust_common.h"
#include "optitrust_models.h"

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

// TODO: side conditions for positive check
__DECL(tree_sum, "(int -> float) * int * int -> int -> float");
__AXIOM(tree_sum_ind, "forall (f: int -> float) (n: int) (i: int) (t: int) (_: t < (1 << (i-1))) -> "
  "tree_sum(f,n,i)(t) +. tree_sum(f,n,i)(t + (1 << (i-1))) =. tree_sum(f,n,i-1)(t)");
__AXIOM(tree_sum_complete, "forall (f: int -> float) (n: int) -> tree_sum(f,n,0)(0) =. reduce_sum(1 << n, f)");
__AXIOM(tree_sum_base, "forall (f: int -> float) (n: int) (t: int) -> f(t) =. tree_sum(f,n,n)(t)");

__AXIOM(shift_distrib, "forall (b: int) (e: int) (_: e > 0) -> (b << e) - (b << (e-1)) = (b << (e-1))");
//__AXIOM(shift_distrib_sym, "forall (b: int) (e: int) (_: e > 0) -> (b << (e-1)) = (b << e) - (b << (e-1))");

// A group with nothing in it is equivalent to the "emp" permission
// TODO: already implemented as group_intro_zero
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
  __requires("logN_geq_0: 0 <= logN");
  __consumes("arr ~> Matrix1((1 << logN), A)");
  __produces("arr ~> UninitMatrix1((1 << logN))");
  __ensures("_Res =. reduce_sum((1 << logN), A)");

  const int N = 1 << logN;

  for (int t = 0; t < N; t++) {
    __xconsumes("&arr[MINDEX1(N,t)] ~~> A(t)");
    __xproduces("&arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,logN)(t)");
    __ghost(rewrite_float_linear, "inside := (fun v -> &arr[MINDEX1(N,t)] ~~> v), by := tree_sum_base(A, logN, t)");
  }

  __ghost(group_intro_empty, "N := N, items := fun t -> &arr[MINDEX1(N,t)] ~> UninitCell");

  for (int i = logN; i > 0; i--) {
    __spreserves("for t in 0..(1<<i) -> &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i)(t)");
    __spreserves("for t in (1<<i)..N -> &arr[MINDEX1(N,t)] ~> UninitCell");
    __strict();
    __ghost(in_range_bounds_rev, "i", "i_gt_0 <- lower_bound, i_leq_logN <- upper_bound");
    __ghost([&] ()   {
      __requires("i > 0");
      __ensures("i_geq_0: i >= 0");
      __requires("i <= logN");
      __ensures("i1_leq_logN: i-1 <= logN");
      __admitted();
      __with("justif := arith_simpl");
    }, "");

    const int ei = (1 << (i - 1));
    const int eii = (1 << i);

    // Split groups
    __ghost(assert_prop, "i-1 <= i", "i1_leq_i <- proof");
    __ghost(expand_subrange, "a := 0, b := ei, s := 1, c := eii, up_ineq := shiftr_monotonic(1,i-1,i,i1_leq_i)");
    __ghost(group_split, "start := 0, stop := eii, split := ei, items := fun t -> &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i)(t)");

    // Introduce IF permission
    __ghost(assert_prop, "i-1 <= logN", "i1_leq_logN <- proof");
    __ghost(group_expand_r_if_intros, "n1 := ei, n2 := N, expand_check := shiftr_monotonic(1,i-1,logN,i1_leq_logN), items := fun t -> &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i)(t)");

    __GHOST_BEGIN(shift, matrix1_span_shift, "a := ei, b := eii, matrix := arr, M := fun t -> tree_sum(A,logN,i)(t)");
    __ghost(rewrite_linear, "inside := fun v -> for t in 0..v -> &(&arr[ei])[MINDEX1(v, t)] ~~> tree_sum(A,logN,i)(t + ei), by := shift_distrib(1,i, i_gt_0)");

    for (int t = 0; t < N; t++) {
      __strict();
      __xconsumes("If(t < (1 << (i - 1)), &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i)(t))");
      __xproduces("If(t < (1 << (i - 1)), &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i-1)(t))");
      __sreads("for t3 in 0..ei -> &(&arr[ei])[MINDEX1(ei,t3)] ~~> tree_sum(A,logN,i)(t3 + ei)");
      __ghost(in_range_bounds, "t", "t_gt_0 <- lower_bound");

      if (t < (1 << (i - 1))) {
        __ghost(bounds_to_in_range, "x := t, a := 0, b := ei");
        __GHOST_BEGIN(focus, ro_matrix1_focus, "&arr[ei], t");
        __ghost(if_then_specialize, "H := &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i)(t)");
        arr[MINDEX1(N,t)] += (&arr[ei])[MINDEX1(ei,t)];
        __GHOST_END(focus);
        __ghost(assert_prop, "t < (1 << (i - 1))", "H3 <- proof");
        __ghost(rewrite_float_linear, "inside := fun v -> &arr[MINDEX1(N,t)] ~~> v, by := tree_sum_ind(A, logN, i, t, H3)");
        __ghost(if_then_unspecialize, "H := &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i-1)(t)");
      } else {
        __ghost(if_else_rewrite, "H2 := &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i-1)(t)");
      }
    }

    __ghost(rewrite_linear, "inside := fun v -> for t in 0..v -> &(&arr[ei])[MINDEX1(v, t)] ~~> tree_sum(A,logN,i)(t + ei), by := eq_sym(eii-ei, ei, shift_distrib(1,i, i_gt_0))");
    __GHOST_END(shift);
    __ghost(group_shrink_r_if_elim, "n1 := (1 << (i - 1)), n2 := N, expand_check := shiftr_monotonic(1,i-1,logN,i1_leq_logN), items := fun t -> &arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,i-1)(t)");
    __ghost([&] ()   {
      __consumes("for t in (1 << (i - 1))..(1 << i) -> &arr[MINDEX1(N, t)] ~~> tree_sum(A, logN, i)(t)");
      __produces("for t in (1 << (i - 1))..(1 << i) -> &arr[MINDEX1(N, t)] ~> UninitCell");
    });
    __ghost(expand_subrange, "a := ei, b := eii, s := 1, c := N, up_ineq := shiftr_monotonic(1,i,logN,i_leq_logN)");
    __ghost(group_join, "start := (1 << (i - 1)), stop := N, split := (1 << i), step := 1,  items := fun t -> &arr[MINDEX1(N,t)] ~> UninitCell");
  }

  __GHOST_BEGIN(focus2, group_focus, "i := 0, items := fun t -> &arr[MINDEX1(N, t)] ~~> tree_sum(A, logN, 0)(t)");
  float sum = arr[MINDEX1(N,0)];
  __GHOST_END(focus2);

  __ghost(expand_subrange, "a := 0, b := 1 << 0, s := 1, c := 1 << logN, up_ineq := shiftr_monotonic(1,0,logN,logN_geq_0)");
  __ghost([&] ()   {
    __consumes("for t in 0..(1<<0) -> &arr[MINDEX1(N, t)] ~~> tree_sum(A, logN, 0)(t)");
    __produces("for t in 0..(1<<0) -> &arr[MINDEX1(N, t)] ~> UninitCell");
  });
  __ghost(group_join, "start := 0, stop := N, split := 1 << 0, items := fun t -> &arr[MINDEX1(N,t)] ~> UninitCell");

  __ghost(assert_prop, "proof := tree_sum_complete(A, logN)");
  return sum;
}
