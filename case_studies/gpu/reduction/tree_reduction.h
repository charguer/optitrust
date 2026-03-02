#include <optitrust_gpu.h>

/* ---- Tree Reductions ---- */

/*

experimenting with proving tree reductions from ground up

__GHOST(rs_index_rewrite) { // A and B instead of A(f) and A(g) ??
  __requires("f: int -> int, g: int -> int, A: int -> float");
  __requires("f_g_eq: forall (i: int) -> f(i) = g(i)");
  __requires("n_in: int");
  __ensures("reduce_sum(n_in,fun i -> A(f(i))) =. reduce_sum(n_in,fun i -> A(g(i)))");

  int n_inpure;
  __ghost([&]() {
    __consumes("&n_inpure ~> UninitCell");
    __produces("&n_inpure ~~> n_in");
    __admitted();
  });
  const int n = n_inpure;

  __DEF(Af, "fun (i: int) -> A(f(i))");
  __DEF(Ag, "fun (i: int) -> A(g(i))");

  __PROOF(H1, "reduce_sum_empty(Af)");
  __ghost(rewrite_float_prop, "inside := fun v -> v =. reduce_sum(0,Af), by := reduce_sum_empty(Ag)", "Hbase <- out");
  __ghost(eq_sym_float, "H := Hbase");

  for (int j = 0; j < n; j++) {
    __srequires("Hind: reduce_sum(j,Af) =. reduce_sum(j,Ag)");
    __ghost(in_range_bounds, "j", "j_geq_0 <- lower_bound");

    __ghost(eq_refl_float, "x := Af(j)");
    __ghost(rewrite_prop, "inside := fun i -> A(f(j)) =. A(i), by := f_g_eq(j)", "Hfg <- out");

    __PROOF(H2, "reduce_sum_add_right(j,Af,j_geq_0)");
    __ghost(rewrite_float_prop, "inside := fun v -> reduce_sum(j,Af) +. v =. reduce_sum(j+1,Af), by := Hfg");
    __ghost(rewrite_float_prop, "inside := fun v -> (v +. Ag(j) =. reduce_sum(j+1,Af)), by := Hind");
    __ghost(rewrite_float_prop, "inside := fun v -> (v =. reduce_sum(j+1,Af)), by := reduce_sum_add_right(j,Ag,j_geq_0)", "Hind1 <- out");
    __ghost(eq_sym_float, "H := Hind1");
  }
}

*/

// TODO: side conditions for positive check
// TODO: rename to reduce_sum_tree
__DECL(tree_sum, "(int -> float) * int * int -> int -> float");
__AXIOM(tree_sum_ind, "forall (f: int -> float) (n: int) (i: int) (t: int) (_: t < (1 << (i-1))) -> "
  "tree_sum(f,n,i)(t) +. tree_sum(f,n,i)(t + (1 << (i-1))) =. tree_sum(f,n,i-1)(t)");
__AXIOM(tree_sum_complete, "forall (f: int -> float) (n: int) -> tree_sum(f,n,0)(0) =. reduce_sum(1 << n, f)");
__AXIOM(tree_sum_base, "forall (f: int -> float) (n: int) (t: int) -> f(t) =. tree_sum(f,n,n)(t)");

__AXIOM(shift_distrib, "forall (b: int) (e: int) (_: e > 0) -> (b << e) - (b << (e-1)) = (b << (e-1))");

float tree_reduce(float *arr, int logN, int N_in) {
  __requires("A: int -> float");
  __requires("logN_geq_0: 0 <= logN");
  __requires("logN_check: N_in = 1 << logN");
  __consumes("arr ~> Matrix1(N_in, A)");
  __produces("arr ~> UninitMatrix1(N_in)");
  __ensures("_Res =. reduce_sum(N_in, A)");

  // re-assert due to inlining problems
  __ASSERT(logN_geq_0_1, "0 <= logN");
  __ASSERT(logN_check_1, "N_in = 1 << logN");

  const int N = 1 << logN;

  __ghost(rewrite_linear, "inside := (fun v -> for i in 0..v -> &arr[MINDEX1(N_in,i)] ~~> A(i)), by := logN_check_1");
  for (int t = 0; t < N; t++) {
    __xconsumes("&arr[MINDEX1(N_in,t)] ~~> A(t)");
    __xproduces("&arr[MINDEX1(N,t)] ~~> tree_sum(A,logN,logN)(t)");
    __ghost(rewrite_float_linear, "inside := (fun v -> &arr[MINDEX1(N_in,t)] ~~> v), by := tree_sum_base(A, logN, t)");
    __ghost(rewrite_linear, "inside := (fun v -> &arr[MINDEX1(v,t)] ~~> tree_sum(A,logN,logN)(t)), by := logN_check_1");
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

    __GHOST_BEGIN(shift, matrix1_span_shift, "a := ei, b := eii, matrix := arr, n1 := N, M := fun t -> tree_sum(A,logN,i)(t)");
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
  float out_sum = arr[MINDEX1(N,0)];
  __GHOST_END(focus2);

  __ghost(expand_subrange, "a := 0, b := 1 << 0, s := 1, c := 1 << logN, up_ineq := shiftr_monotonic(1,0,logN,logN_geq_0_1)");
  __ghost([&] ()   {
    __consumes("for t in 0..(1<<0) -> &arr[MINDEX1(N, t)] ~~> tree_sum(A, logN, 0)(t)");
    __produces("for t in 0..(1<<0) -> &arr[MINDEX1(N, t)] ~> UninitCell");
  });
  __ghost(group_join, "start := 0, stop := N, split := 1 << 0, items := fun t -> &arr[MINDEX1(N,t)] ~> UninitCell");

  for (int t = 0; t < N; t++) {
    __xconsumes("&arr[MINDEX1(N,t)] ~> UninitCell");
    __xproduces("&arr[MINDEX1(N_in,t)] ~> UninitCell");
    __ghost(rewrite_linear, "inside := (fun v -> &arr[MINDEX1(v,t)] ~> UninitCell), by := eq_sym(N_in,N,logN_check_1)");
  }
  __ghost(rewrite_linear, "inside := (fun v -> for i in 0..v -> &arr[MINDEX1(N_in,i)] ~> UninitCell), by := eq_sym(N_in,N,logN_check_1)");
  __ghost(assert_prop, "proof := tree_sum_complete(A, logN)");
  __ghost(rewrite_prop, "inside := fun v -> tree_sum(A, logN, 0)(0) =. reduce_sum(v, A), by := eq_sym(N_in,N,logN_check_1)");

  return out_sum;
}
