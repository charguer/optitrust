#include <optitrust.h>



typedef uint8_t T;

typedef uint16_t ST;

  void rowSum_cn1 (   const  int kn,  const T* S, ST* D,   const  int n
)  {
  __modifies("D ~> Matrix2(n, 1)");
  __reads("S ~> Matrix2(n + kn, 1)");
  __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, 1, i, c)] ~> Cell");
  __ghost([&] (  )   {
    __consumes("Group(range(0, 1, 1), fun c -> Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 1, i, c)] ~> Cell))");
    __produces("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 1, i, 0)] ~> Cell)");
    __admitted();
    __with("justif := unroll");
  }, "");
  ST s = 0;
  for ( int i = 0; i < kn; i++ ) {
    __sequentially_modifies("&s ~> Cell");
    __parallel_reads("S ~> Matrix2(n + kn, 1)");
      const  __ghost_fn sf = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 0");
    s += ( ST ) S[MINDEX2(n + kn, 1, i, 0)];
    __ghost_end(sf);
  }
    const  __ghost_fn df = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 1, i, 0)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  D[MINDEX2(n, 1, 0, 0)] = s;
  __ghost_end(df);
  for ( int i = 0; i < n - 1; i++ ) {
    __sequentially_modifies("&s ~> Cell");
    __sequentially_modifies("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 1, i, 0)] ~> Cell)");
    __parallel_reads("S ~> Matrix2(n + kn, 1)");
      const  __ghost_fn sf1 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 0");
    s -= ( ST ) S[MINDEX2(n + kn, 1, i, 0)];
    __ghost_end(sf1);
      const  __ghost_fn sf2 = __ghost_begin(matrix2_ro_focus, "M := S, i := i + kn, j := 0");
    s += ( ST ) S[MINDEX2(n + kn, 1, i + kn, 0)];
    __ghost_end(sf2);
      const  __ghost_fn df = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 1, i, 0)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
    D[MINDEX2(n, 1, i + 1, 0)] = s;
    __ghost_end(df);
  }
  __ghost([&] (  )   {
    __consumes("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 1, i, 0)] ~> Cell)");
    __produces("Group(range(0, 1, 1), fun c -> Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 1, i, c)] ~> Cell))");
    __admitted();
    __with("justif := roll");
  }, "");
  __ghost(swap_groups_rev, "items := fun i, c -> &D[MINDEX2(n, 1, i, c)] ~> Cell");
}

  void rowSum_cn3 (   const  int kn,  const T* S, ST* D,   const  int n
)  {
  __modifies("D ~> Matrix2(n, 3)");
  __reads("S ~> Matrix2(n + kn, 3)");
  __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, 3, i, c)] ~> Cell");
  __ghost([&] (  )   {
    __consumes("Group(range(0, 3, 1), fun c -> Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, c)] ~> Cell))");
    __produces("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell)");
    __produces("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell)");
    __produces("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell)");
    __admitted();
    __with("justif := unroll");
  }, "");
  ST s = 0;
  ST s1 = 0;
  ST s3 = 0;
  for ( int i = 0; i < kn; i++ ) {
      const  __ghost_fn sf = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 0");
    s += ( ST ) S[MINDEX2(n + kn, 3, i, 0)];
    __ghost_end(sf);
      const  __ghost_fn sf12 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 1");
    s1 += ( ST ) S[MINDEX2(n + kn, 3, i, 1)];
    __ghost_end(sf12);
      const  __ghost_fn sf11 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 2");
    s3 += ( ST ) S[MINDEX2(n + kn, 3, i, 2)];
    __ghost_end(sf11);
  }
    const  __ghost_fn __ghost_pair_2 = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  D[MINDEX2(n, 3, 0, 0)] = s;
  __ghost_end(__ghost_pair_2);
    const  __ghost_fn __ghost_pair_4 = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  D[MINDEX2(n, 3, 0, 1)] = s1;
  __ghost_end(__ghost_pair_4);
    const  __ghost_fn __ghost_pair_6 = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
  D[MINDEX2(n, 3, 0, 2)] = s3;
  __ghost_end(__ghost_pair_6);
  for ( int i = 0; i < n - 1; i++ ) {
      const  __ghost_fn sf1 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 0");
    s -= ( ST ) S[MINDEX2(n + kn, 3, i, 0)];
    __ghost_end(sf1);
      const  __ghost_fn sf2 = __ghost_begin(matrix2_ro_focus, "M := S, i := i + kn, j := 0");
    s += ( ST ) S[MINDEX2(n + kn, 3, i + kn, 0)];
    __ghost_end(sf2);
      const  __ghost_fn df = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
    D[MINDEX2(n, 3, i + 1, 0)] = s;
    __ghost_end(df);
      const  __ghost_fn sf18 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 1");
    s1 -= ( ST ) S[MINDEX2(n + kn, 3, i, 1)];
    __ghost_end(sf18);
      const  __ghost_fn sf29 = __ghost_begin(matrix2_ro_focus, "M := S, i := i + kn, j := 1");
    s1 += ( ST ) S[MINDEX2(n + kn, 3, i + kn, 1)];
    __ghost_end(sf29);
      const  __ghost_fn df10 = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
    D[MINDEX2(n, 3, i + 1, 1)] = s1;
    __ghost_end(df10);
      const  __ghost_fn sf15 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := 2");
    s3 -= ( ST ) S[MINDEX2(n + kn, 3, i, 2)];
    __ghost_end(sf15);
      const  __ghost_fn sf26 = __ghost_begin(matrix2_ro_focus, "M := S, i := i + kn, j := 2");
    s3 += ( ST ) S[MINDEX2(n + kn, 3, i + kn, 2)];
    __ghost_end(sf26);
      const  __ghost_fn df7 = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
    D[MINDEX2(n, 3, i + 1, 2)] = s3;
    __ghost_end(df7);
  }
  __ghost([&] (  )   {
    __consumes("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 0)] ~> Cell)");
    __consumes("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 1)] ~> Cell)");
    __consumes("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, 2)] ~> Cell)");
    __produces("Group(range(0, 3, 1), fun c -> Group(range(0, n, 1), fun i -> &D[MINDEX2(n, 3, i, c)] ~> Cell))");
    __admitted();
    __with("justif := roll");
  }, "");
  __ghost(swap_groups_rev, "items := fun i, c -> &D[MINDEX2(n, 3, i, c)] ~> Cell");
}

  void rowSumOpt (   const  int kn,  const T* S, ST* D,   const  int n,   const  int cn
)  {
  if ( cn == 1 ) { rowSum_cn1(kn, S, D, n); }
  else if ( cn == 3 ) { rowSum_cn3(kn, S, D, n); }
  else {
    __ghost(swap_groups, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
    for ( int c = 0; c < cn; c++ ) {
      __parallel_reads("S ~> Matrix2(n + kn, cn)");
      __modifies("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell)");
      ST s = 0;
      for ( int i = 0; i < kn; i++ ) {
        __sequentially_modifies("&s ~> Cell");
        __parallel_reads("S ~> Matrix2(n + kn, cn)");
          const  __ghost_fn sf = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := c");
        s += ( ST ) S[MINDEX2(n + kn, cn, i, c)];
        __ghost_end(sf);
      }
        const  __ghost_fn df = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := 0, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
      D[MINDEX2(n, cn, 0, c)] = s;
      __ghost_end(df);
      for ( int i = 0; i < n - 1; i++ ) {
        __sequentially_modifies("&s ~> Cell");
        __sequentially_modifies("Group(range(0, n, 1), fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell)");
        __parallel_reads("S ~> Matrix2(n + kn, cn)");
          const  __ghost_fn sf1 = __ghost_begin(matrix2_ro_focus, "M := S, i := i, j := c");
        s -= ( ST ) S[MINDEX2(n + kn, cn, i, c)];
        __ghost_end(sf1);
          const  __ghost_fn sf2 = __ghost_begin(matrix2_ro_focus, "M := S, i := i + kn, j := c");
        s += ( ST ) S[MINDEX2(n + kn, cn, i + kn, c)];
        __ghost_end(sf2);
          const  __ghost_fn df = __ghost_begin(group_focus, "items := fun i -> &D[MINDEX2(n, cn, i, c)] ~> Cell, i := i + 1, bound_check_start := checked, bound_check_stop := checked, bound_check_step := checked");
        D[MINDEX2(n, cn, i + 1, c)] = s;
        __ghost_end(df);
      }
    }
    __ghost(swap_groups_rev, "items := fun i, c -> &D[MINDEX2(n, cn, i, c)] ~> Cell");
  }
}
