#include <optitrust_gpu.h>

__ghost(assert_inhabited, "x := arbitrary(int * (int -> float) -> float)",
        "reduce_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> float) -> (0.f =. reduce_sum(0, f)))",
        "reduce_sum_empty <- proof");

__ghost(assert_prop,
        "proof := admit(forall (n: int) (f: int -> float) (_: (n >= 0)) -> "
        "(reduce_sum(n, f) +. f(n) =. reduce_sum(n + 1, f)))",
        "reduce_sum_add_right <- proof");

__ghost(assert_inhabited,
        "x := arbitrary((int -> float) * int * int -> int -> float)",
        "tree_sum <- x");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> float) (n: int) (i: int) (t: int) "
        "(_: (t < 1 << i - 1)) -> (tree_sum(f, n, i)(t) +. tree_sum(f, n, i)(t "
        "+ (1 << i - 1)) =. tree_sum(f, n, i - 1)(t)))",
        "tree_sum_ind <- proof");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> float) (n: int) -> (tree_sum(f, n, "
        "0)(0) =. reduce_sum(1 << n, f)))",
        "tree_sum_complete <- proof");

__ghost(assert_prop,
        "proof := admit(forall (f: int -> float) (n: int) (t: int) -> (f(t) =. "
        "tree_sum(f, n, n)(t)))",
        "tree_sum_base <- proof");

__ghost(assert_prop,
        "proof := admit(forall (b: int) (e: int) (_: (e > 0)) -> ((b << e) - "
        "(b << e - 1) = b << e - 1))",
        "shift_distrib <- proof");

float reduce(float* arr, int N) {
  __requires("A: int -> float");
  __ensures("(_Res =. reduce_sum(N, A))");
  __preserves("HostCtx");
  __reads("arr ~> Matrix1(N, A)");
  __ghost(assert_prop, "P := (N = exact_div(N, 512) * 512)",
          "tile_div_check_i <- proof");
  __ghost(eq_refl_float, "x := reduce_sum(N, A)");
  float sum = 0.f;
  __ghost(rewrite_float_linear,
          "inside := fun v -> &sum ~~> v, by := reduce_sum_empty(A)");
  __ghost(rewrite_linear,
          "inside := fun (i: int) -> &sum ~~> reduce_sum(i, A), by := "
          "zero_mul_intro(512)");
  float* const d_arr = __gmem_malloc1<float>(N);
  __with("T := float");
  __ghost([&]() {
    __preserves("d_arr ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  memcpy_host_to_device1(d_arr, arr, N);
  float* const partial_sums =
      (float*)malloc(MSIZE1(exact_div(N, 512)) * sizeof(float));
  float* const d_partial_sums = __gmem_malloc1<float>(exact_div(N, 512));
  __with("T := float");
  /*@kernel_sequence*/ {
    kernel_launch(MSIZE1(exact_div(N, 512)), MSIZE1(256),
                  sizeof(float) * 256 + 0);
    __ghost(assume,
            "P := (MSIZE1(exact_div(N, 512)) * MSIZE1(256) = "
            "MSIZE2(exact_div(N, 512), 256))");
    __ghost(take_smem_token, "tok_sz := sizeof(float) * 256");
    float* const tile = __smem_malloc1<float>(256);
    __with("T := float");
    __ghost(assume, "P := (MSIZE1(exact_div(N, 512)) = exact_div(N, 512))");
    __ghost(
        rewrite_linear,
        "from := MSIZE1(exact_div(N, 512)), to := exact_div(N, 512), inside := "
        "fun (sz: int) -> desync_for i in ..sz -> for i1 in 0..256 -> "
        "&tile[MINDEX2(sz, 256, DMINDEX1(sz, i), i1)] ~> UninitCellOf(SMem)");
    __ghost([&]() {
      __preserves("d_partial_sums ~> UninitMatrix1Of(exact_div(N, 512), GMem)");
      __admitted();
      __with("justif := shift_groups");
    });
    kernel_setup_end();
    __with("grid_sz := MSIZE2(exact_div(N, 512), 256)");
    __threadfor;
    for (int bi = 0; bi < exact_div(N, 512); bi++) {
      __sreads("d_arr ~> Matrix1Of(N, GMem, A)");
      __xconsumes(
          "for _v11 in 0..256 -> &tile[MINDEX2(exact_div(N, 512), 256, "
          "DMINDEX1(exact_div(N, 512), bi), _v11)] ~> UninitCellOf(SMem)");
      __xconsumes(
          "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~> "
          "UninitCellOf(GMem)");
      __xproduces(
          "desync_for ti_f in ..256 -> If((ti_f = 0), "
          "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] "
          "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
          "reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0))");
      __xproduces(
          "desync_for ti_f in ..256 -> If((ti_f = 0), "
          "&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, 512), "
          "bi), 0)] ~> UninitMatrix1Of(256, SMem))");
      __ghost(assert_prop, "P := (512 = 256 * 2)",
              "tile_div_check_i1 <- proof");
      __threadfor;
      for (int ti = 0; ti < 256; ti++) {
        __sreads("d_arr ~> Matrix1Of(N, GMem, A)");
        __xwrites(
            "&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, 512), "
            "bi), ti)] ~~>[SMem] reduce_sum(bi * 512 + (ti + 1) * 2, A) -. "
            "reduce_sum(bi * 512 + ti * 2, A)");
        __smem_set(&tile[MINDEX2(exact_div(N, 512), 256,
                                 DMINDEX1(exact_div(N, 512), bi), ti)],
                   ({
                     __rewrite_sequence;
                     ({
                       float arith_res = 0.f;
                       __ghost([&]() {
                         __requires("res: float");
                         __consumes("&arith_res ~~> res");
                         __produces(
                             "&arith_res ~~> reduce_sum(bi * 512 + ti * 2, A) "
                             "-. reduce_sum(bi * 512 + ti * 2, A)");
                         __admitted();
                       });
                       const float arith_res3 = arith_res;
                       arith_res3;
                     });
                   }));
        __ghost(rewrite_linear,
                "inside := fun (i: int) -> &tile[MINDEX2(exact_div(N, 512), "
                "256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
                "reduce_sum(bi * 512 + i, A) -. reduce_sum(bi * 512 + ti * 2, "
                "A), by := plus_zero_intro(ti * 2)");
        for (int i = 0; i < 2; i++) {
          __spreserves(
              "&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), ti)] ~~>[SMem] reduce_sum(bi * 512 + (ti * 2 + i), "
              "A) -. reduce_sum(bi * 512 + ti * 2, A)");
          __sreads("d_arr ~> Matrix1Of(N, GMem, A)");
          __ghost(
              tiled_index_in_range,
              "tile_index := ti, index := i, div_check := tile_div_check_i1");
          __ghost(tiled_index_in_range,
                  "tile_index := bi, index := ti * 2 + i, div_check := "
                  "tile_div_check_i");
          const __ghost_fn focusArr =
              __ghost_begin(ro_matrix1_focus,
                            "matrix := d_arr, i := bi * 512 + (ti * 2 + i)");
          __smem_set(
              &tile[MINDEX2(exact_div(N, 512), 256,
                            DMINDEX1(exact_div(N, 512), bi), ti)],
              ({
                __rewrite_sequence;
                ({
                  float arith_res =
                      __smem_get(
                          &tile[MINDEX2(exact_div(N, 512), 256,
                                        DMINDEX1(exact_div(N, 512), bi), ti)]) +
                      __gmem_get(&d_arr[MINDEX1(N, bi * 512 + (ti * 2 + i))]);
                  __ghost([&]() {
                    __requires("res: float");
                    __consumes("&arith_res ~~> res");
                    __produces(
                        "&arith_res ~~> reduce_sum(bi * 512 + (ti * 2 + i), A) "
                        "-. reduce_sum(bi * 512 + ti * 2, A) +. reduce_sum(bi "
                        "* 512 + ti * 2, A) +. A(bi * 512 + (ti * 2 + i)) -. "
                        "reduce_sum(bi * 512 + ti * 2, A)");
                    __admitted();
                  });
                  const float arith_res4 = arith_res;
                  arith_res4;
                });
              }));
          __ghost([&]() {
            __preserves(
                "&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
                "512), bi), ti)] ~~>[SMem] reduce_sum(bi * 512 + (ti * 2 + i), "
                "A) -. reduce_sum(bi * 512 + ti * 2, A) +. reduce_sum(bi * 512 "
                "+ ti * 2, A) +. A(bi * 512 + (ti * 2 + i)) -. reduce_sum(bi * "
                "512 + ti * 2, A)");
            __admitted();
          });
          __ghost(rewrite_float_linear,
                  "inside := fun (v: float) -> &tile[MINDEX2(exact_div(N, "
                  "512), 256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
                  "v +. A(bi * 512 + (ti * 2 + i)) -. reduce_sum(bi * 512 + ti "
                  "* 2, A), by := r_cancel_minus_plus(reduce_sum(bi * 512 + "
                  "(ti * 2 + i), A), reduce_sum(bi * 512 + ti * 2, A))");
          __ghost_end(focusArr);
          __ghost(in_range_bounds, "x := bi * 512 + (ti * 2 + i)",
                  "i_geq_0 <- lower_bound");
          __ghost(rewrite_float_linear,
                  "inside := fun v -> &tile[MINDEX2(exact_div(N, 512), 256, "
                  "DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] v -. "
                  "reduce_sum(bi * 512 + ti * 2, A), by := "
                  "reduce_sum_add_right(bi * 512 + (ti * 2 + i), A, i_geq_0)");
          __ghost(rewrite_linear,
                  "inside := fun (i: int) -> &tile[MINDEX2(exact_div(N, 512), "
                  "256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
                  "reduce_sum(i, A) -. reduce_sum(bi * 512 + ti * 2, A), by := "
                  "add_assoc_right(bi * 512, ti * 2 + i, 1)");
          __ghost(rewrite_linear,
                  "inside := fun (i: int) -> &tile[MINDEX2(exact_div(N, 512), "
                  "256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
                  "reduce_sum(bi * 512 + i, A) -. reduce_sum(bi * 512 + ti * "
                  "2, A), by := add_assoc_right(ti * 2, i, 1)");
        }
        __ghost(rewrite_linear,
                "inside := fun (i: int) -> &tile[MINDEX2(exact_div(N, 512), "
                "256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
                "reduce_sum(bi * 512 + i, A) -. reduce_sum(bi * 512 + ti * 2, "
                "A), by := mul_add_factor(ti, 2)");
      }
      __barrier_sequence;
      {
        blocksync();
        __with(
            "H := desync_for ti in ..256 -> &tile[MINDEX2(exact_div(N, 512), "
            "256, DMINDEX1(exact_div(N, 512), bi), ti)] ~~>[SMem] "
            "reduce_sum(bi * 512 + (ti + 1) * 2, A) -. reduce_sum(bi * 512 + "
            "ti * 2, A)");
      }
      float* const reduce_arr_12 = &tile[MINDEX2(
          exact_div(N, 512), 256, DMINDEX1(exact_div(N, 512), bi), 0)];
      __ghost(assert_prop, "P := (256 = 1 << 8)", "logN_check_1 <- proof");
      __ghost(rewrite_linear,
              "inside := fun v -> for i in 0..v -> &reduce_arr_12[MINDEX1(256, "
              "i)] ~~>[SMem] reduce_sum(bi * 512 + (i + 1) * 2, A) -. "
              "reduce_sum(bi * 512 + i * 2, A), by := logN_check_1");
      for (int t = 0; t < 1 << 8; t++) {
        __xconsumes(
            "&reduce_arr_12[MINDEX1(256, t)] ~~>[SMem] reduce_sum(bi * 512 + "
            "(t + 1) * 2, A) -. reduce_sum(bi * 512 + t * 2, A)");
        __xproduces(
            "&reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] tree_sum(fun (i1: "
            "int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. reduce_sum(bi * "
            "512 + i1 * 2, A), 8, 8)(t)");
        __ghost(rewrite_float_linear,
                "inside := fun v -> &reduce_arr_12[MINDEX1(256, t)] ~~>[SMem] "
                "v, by := tree_sum_base(fun (i1: int) -> reduce_sum(bi * 512 + "
                "(i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, t)");
        __ghost(rewrite_linear,
                "inside := fun v -> &reduce_arr_12[MINDEX1(v, t)] ~~>[SMem] "
                "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, "
                "A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 8)(t), by := "
                "logN_check_1");
      }
      __ghost(group_intro_empty,
              "N := 1 << 8, items := fun t -> &reduce_arr_12[MINDEX1(1 << 8, "
              "t)] ~> UninitCellOf(SMem)");
      for (int i = 8; i > 0; i--) {
        __spreserves(
            "ThreadsCtx(MINDEX2(exact_div(N, 512), 0, bi, 0)..+MSIZE1(256))");
        __spreserves(
            "for t in 0..(1 << i) -> &reduce_arr_12[MINDEX1(1 << 8, t)] "
            "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
            "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t)");
        __spreserves(
            "for t in (1 << i)..(1 << 8) -> &reduce_arr_12[MINDEX1(1 << 8, t)] "
            "~> UninitCellOf(SMem)");
        __ghost(in_range_bounds_rev, "x := i",
                "i_gt_0 <- lower_bound, i_leq_logN <- upper_bound");
        __ghost([&]() {
          __requires("(i > 0)");
          __requires("(i <= 8)");
          __ensures("i_geq_0: (i >= 0)");
          __ensures("i1_leq_logN: (i - 1 <= 8)");
          __admitted();
          __with("justif := arith_simpl");
        });
        const int ei = 1 << i - 1;
        const int eii = 1 << i;
        __ghost(assert_prop, "P := (i - 1 <= i)", "i1_leq_i <- proof");
        __ghost(expand_subrange,
                "a := 0, b := ei, s := 1, c := eii, up_ineq := "
                "shiftr_monotonic(1, i - 1, i, i1_leq_i)");
        __ghost(group_split,
                "start := 0, stop := eii, split := ei, items := fun t -> "
                "&reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] tree_sum(fun "
                "(i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
                "reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t)");
        __ghost(assert_prop, "P := (i - 1 <= 8)", "i1_leq_logN <- proof");
        __ghost(
            group_expand_r_if_intros,
            "n1 := ei, n2 := 1 << 8, expand_check := shiftr_monotonic(1, i - "
            "1, 8, i1_leq_logN), items := fun t -> &reduce_arr_12[MINDEX1(1 << "
            "8, t)] ~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + "
            "(i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t)");
        const __ghost_fn shift = __ghost_begin(
            matrix1_span_shift,
            "a := ei, b := eii, matrix := reduce_arr_12, n1 := 1 << 8, M := "
            "fun t -> tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) "
            "* 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t)");
        __ghost(rewrite_linear,
                "inside := fun v -> for t in 0..v -> "
                "&(&reduce_arr_12[ei])[MINDEX1(v, t)] ~~>[SMem] tree_sum(fun "
                "(i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
                "reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t + ei), by := "
                "shift_distrib(1, i, i_gt_0)");
        __ghost(rewrite_threadsctx_sz1, "from := 256, to := 1 << 8");
        __threadfor;
        for (int t = 0; t < 1 << 8; t++) {
          __sreads(
              "for t3 in 0..ei -> &(&reduce_arr_12[ei])[MINDEX1(ei, t3)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t3 + ei)");
          __xconsumes(
              "If((t < 1 << i - 1), &reduce_arr_12[MINDEX1(1 << 8, t)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t))");
          __xproduces(
              "If((t < 1 << i - 1), &reduce_arr_12[MINDEX1(1 << 8, t)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i - 1)(t))");
          __ghost(in_range_bounds, "x := t", "t_gt_0 <- lower_bound");
          if (t < 1 << i - 1) {
            __ghost(bounds_to_in_range, "x := t, a := 0, b := ei");
            const __ghost_fn focus = __ghost_begin(
                ro_matrix1_focus, "matrix := &reduce_arr_12[ei], i := t");
            __ghost(if_then_specialize,
                    "H := &reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] "
                    "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) "
                    "* 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t)");
            __smem_set(&reduce_arr_12[MINDEX1(1 << 8, t)],
                       __smem_get(&reduce_arr_12[MINDEX1(1 << 8, t)]) +
                           __smem_get(&(&reduce_arr_12[ei])[MINDEX1(ei, t)]));
            __ghost_end(focus);
            __ghost(assert_prop, "P := (t < 1 << i - 1)", "H3 <- proof");
            __ghost(rewrite_float_linear,
                    "inside := fun v -> &reduce_arr_12[MINDEX1(1 << 8, t)] "
                    "~~>[SMem] v, by := tree_sum_ind(fun (i1: int) -> "
                    "reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. reduce_sum(bi * "
                    "512 + i1 * 2, A), 8, i, t, H3)");
            __ghost(
                if_then_unspecialize,
                "H := &reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] "
                "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, "
                "A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i - 1)(t)");
          } else {
            __ghost(
                if_else_rewrite,
                "H2 := &reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] "
                "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, "
                "A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, i - 1)(t)");
          }
        }
        __ghost(rewrite_threadsctx_sz1, "from := 1 << 8, to := 256");
        __barrier_sequence;
        {
          blocksync();
          __with(
              "H := desync_for t in ..1 << 8 -> If((t < 1 << i - 1), "
              "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), 0)])[MINDEX1(1 << 8, t)] ~~>[SMem] tree_sum(fun (i1: "
              "int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. reduce_sum(bi "
              "* 512 + i1 * 2, A), 8, i - 1)(t))");
        }
        __ghost(rewrite_linear,
                "inside := fun v -> for t in 0..v -> "
                "&(&reduce_arr_12[ei])[MINDEX1(v, t)] ~~>[SMem] tree_sum(fun "
                "(i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
                "reduce_sum(bi * 512 + i1 * 2, A), 8, i)(t + ei), by := "
                "eq_sym(eii - ei, ei, shift_distrib(1, i, i_gt_0))");
        __ghost_end(shift);
        __ghost(group_shrink_r_if_elim,
                "n1 := 1 << i - 1, n2 := 1 << 8, expand_check := "
                "shiftr_monotonic(1, i - 1, 8, i1_leq_logN), items := fun t -> "
                "&reduce_arr_12[MINDEX1(1 << 8, t)] ~~>[SMem] tree_sum(fun "
                "(i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
                "reduce_sum(bi * 512 + i1 * 2, A), 8, i - 1)(t)");
        __ghost([&]() {
          __consumes(
              "for t in (1 << i - 1)..(1 << i) -> &reduce_arr_12[MINDEX1(1 << "
              "8, t)] ~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 "
              "+ (i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, "
              "i)(t)");
          __produces(
              "for t in (1 << i - 1)..(1 << i) -> &reduce_arr_12[MINDEX1(1 << "
              "8, t)] ~> UninitCellOf(SMem)");
        });
        __ghost(expand_subrange,
                "a := ei, b := eii, s := 1, c := 1 << 8, up_ineq := "
                "shiftr_monotonic(1, i, 8, i_leq_logN)");
        __ghost(group_join,
                "start := 1 << i - 1, stop := 1 << 8, split := 1 << i, step := "
                "1, items := fun t -> &reduce_arr_12[MINDEX1(1 << 8, t)] ~> "
                "UninitCellOf(SMem)");
      }
      __ghost(group_singleton_if_intros,
              "n := 256, H := &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] "
              "~> UninitCellOf(GMem)");
      __ghost(group_singleton_if_intros,
              "n := 256, H := for t in (1 << 0)..(1 << 8) -> "
              "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), 0)])[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem)");
      __ghost(
          group_singleton_if_intros,
          "n := 256, H := for t in 0..(1 << 0) -> &(&tile[MINDEX2(exact_div(N, "
          "512), 256, DMINDEX1(exact_div(N, 512), bi), 0)])[MINDEX1(1 << 8, "
          "t)] ~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
          "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(t)");
      __ghost(assert_prop, "P := (0 <= 8)", "logN_geq_0_1 <- proof");
      __threadfor;
      for (int ti_f = 0; ti_f < 256; ti_f += 1) {
        __xconsumes(
            "If((ti_f = 0), for t in 0..(1 << 0) -> "
            "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
            "512), bi), 0)])[MINDEX1(1 << 8, t)] ~~>[SMem] tree_sum(fun (i1: "
            "int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. reduce_sum(bi * "
            "512 + i1 * 2, A), 8, 0)(t))");
        __xconsumes(
            "If((ti_f = 0), for t in (1 << 0)..(1 << 8) -> "
            "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
            "512), bi), 0)])[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem))");
        __xconsumes(
            "If((ti_f = 0), &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~> "
            "UninitCellOf(GMem))");
        __xproduces(
            "If((ti_f = 0), &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] "
            "~~>[GMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
            "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0))");
        __xproduces(
            "If((ti_f = 0), &tile[MINDEX2(exact_div(N, 512), 256, "
            "DMINDEX1(exact_div(N, 512), bi), 0)] ~> UninitMatrix1Of(256, "
            "SMem))");
        if (ti_f == 0) {
          __ghost(
              if_then_specialize,
              "H := for t in 0..(1 << 0) -> &(&tile[MINDEX2(exact_div(N, 512), "
              "256, DMINDEX1(exact_div(N, 512), bi), 0)])[MINDEX1(1 << 8, t)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(t)");
          __ghost(
              if_then_specialize,
              "H := for t in (1 << 0)..(1 << 8) -> "
              "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), 0)])[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem)");
          __ghost(if_then_specialize,
                  "H := &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~> "
                  "UninitCellOf(GMem)");
          const __ghost_fn focus2 = __ghost_begin(
              group_focus,
              "i := 0, items := fun t -> &reduce_arr_12[MINDEX1(1 << 8, t)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(t)");
          const float sum_temp_13 =
              __smem_get(&reduce_arr_12[MINDEX1(1 << 8, 0)]);
          __ghost_end(focus2);
          __ghost(expand_subrange,
                  "a := 0, b := 1 << 0, s := 1, c := 1 << 8, up_ineq := "
                  "shiftr_monotonic(1, 0, 8, logN_geq_0_1)");
          __ghost([&]() {
            __consumes(
                "for t in 0..(1 << 0) -> &reduce_arr_12[MINDEX1(1 << 8, t)] "
                "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 "
                "+ 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(t)");
            __produces(
                "for t in 0..(1 << 0) -> &reduce_arr_12[MINDEX1(1 << 8, t)] ~> "
                "UninitCellOf(SMem)");
          });
          __ghost(
              group_join,
              "start := 0, stop := 1 << 8, split := 1 << 0, items := fun t -> "
              "&reduce_arr_12[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem)");
          for (int t = 0; t < 1 << 8; t++) {
            __xconsumes(
                "&reduce_arr_12[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem)");
            __xproduces(
                "&reduce_arr_12[MINDEX1(256, t)] ~> UninitCellOf(SMem)");
            __ghost(
                rewrite_linear,
                "inside := fun v -> &reduce_arr_12[MINDEX1(v, t)] ~> "
                "UninitCellOf(SMem), by := eq_sym(256, 1 << 8, logN_check_1)");
          }
          __ghost(rewrite_linear,
                  "inside := fun v -> for i in 0..v -> "
                  "&reduce_arr_12[MINDEX1(256, i)] ~> UninitCellOf(SMem), by "
                  ":= eq_sym(256, 1 << 8, logN_check_1)");
          __ghost(
              assert_prop,
              "proof := tree_sum_complete(fun (i1: int) -> reduce_sum(bi * 512 "
              "+ (i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8)");
          __ghost(
              rewrite_prop,
              "inside := fun v -> (tree_sum(fun (i1: int) -> reduce_sum(bi * "
              "512 + (i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, "
              "0)(0) =. reduce_sum(v, fun (i1: int) -> reduce_sum(bi * 512 + "
              "(i1 + 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A))), by := "
              "eq_sym(256, 1 << 8, logN_check_1)");
          __gmem_set(&d_partial_sums[MINDEX1(exact_div(N, 512), bi)],
                     sum_temp_13);
          __ghost(
              if_then_unspecialize,
              "H := &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] "
              "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, "
              "A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0)");
          __ghost(if_then_unspecialize,
                  "H := &tile[MINDEX2(exact_div(N, 512), 256, "
                  "DMINDEX1(exact_div(N, 512), bi), 0)] ~> "
                  "UninitMatrix1Of(256, SMem)");
        } else {
          __ghost(
              if_else_rewrite,
              "H := for t in 0..(1 << 0) -> &(&tile[MINDEX2(exact_div(N, 512), "
              "256, DMINDEX1(exact_div(N, 512), bi), 0)])[MINDEX1(1 << 8, t)] "
              "~~>[SMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(t), H2 := "
              "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] "
              "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, "
              "A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0)");
          __ghost(
              if_else_rewrite,
              "H := for t in (1 << 0)..(1 << 8) -> "
              "&(&tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), 0)])[MINDEX1(1 << 8, t)] ~> UninitCellOf(SMem), H2 "
              ":= &tile[MINDEX2(exact_div(N, 512), 256, DMINDEX1(exact_div(N, "
              "512), bi), 0)] ~> UninitMatrix1Of(256, SMem)");
          __ghost(if_else_drop,
                  "H := &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~> "
                  "UninitCellOf(GMem)");
        }
      }
    }
    kernel_teardown_begin();
    __with("grid_sz := MSIZE2(exact_div(N, 512), 256)");
    __barrier_sequence;
    {
      __ghost(
          kernel_teardown_sync,
          "H := desync_for bi in ..exact_div(N, 512) -> desync_for ti_f in "
          "..256 -> If((ti_f = 0), &d_partial_sums[MINDEX1(exact_div(N, 512), "
          "bi)] ~~>[GMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 "
          "+ 1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0))");
      __ghost(kernel_teardown_sync,
              "H := desync_for bi in ..exact_div(N, 512) -> desync_for ti_f in "
              "..256 -> If((ti_f = 0), &tile[MINDEX2(exact_div(N, 512), 256, "
              "DMINDEX1(exact_div(N, 512), bi), 0)] ~> UninitMatrix1Of(256, "
              "SMem))");
    }
    for (int bi = 0; bi < exact_div(N, 512); bi++) {
      __xconsumes(
          "for ti_f in 0..256 -> If((ti_f = 0), "
          "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] "
          "tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + 1) * 2, A) -. "
          "reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0))");
      __xconsumes(
          "for ti_f in 0..256 -> If((ti_f = 0), &tile[MINDEX2(exact_div(N, "
          "512), 256, DMINDEX1(exact_div(N, 512), bi), 0)] ~> "
          "UninitMatrix1Of(256, SMem))");
      __xproduces(
          "for _v11 in 0..256 -> &tile[MINDEX2(exact_div(N, 512), 256, "
          "DMINDEX1(exact_div(N, 512), bi), _v11)] ~> UninitCellOf(SMem)");
      __xproduces(
          "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] "
          "reduce_sum((bi + 1) * 512, A) -. reduce_sum(bi * 512, A)");
      __ghost(
          group_singleton_if_elim,
          "n := 256, H := &tile[MINDEX2(exact_div(N, 512), 256, "
          "DMINDEX1(exact_div(N, 512), bi), 0)] ~> UninitMatrix1Of(256, SMem)");
      __ghost(group_singleton_if_elim,
              "n := 256, H := &d_partial_sums[MINDEX1(exact_div(N, 512), bi)] "
              "~~>[GMem] tree_sum(fun (i1: int) -> reduce_sum(bi * 512 + (i1 + "
              "1) * 2, A) -. reduce_sum(bi * 512 + i1 * 2, A), 8, 0)(0)");
      __ghost(rewrite_float_linear_admitted,
              "to := reduce_sum(bi * 512 + 256 * 2, A) -. reduce_sum(bi * 512, "
              "A), inside := fun (v: float) -> "
              "&d_partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~>[GMem] v");
      __ghost(assert_prop, "P := (512 = 256 * 2)",
              "tile_div_check_i115 <- proof");
      __ghost(
          rewrite_linear,
          "inside := fun (i: int) -> &d_partial_sums[MINDEX1(exact_div(N, "
          "512), bi)] ~~>[GMem] reduce_sum(bi * 512 + i, A) -. reduce_sum(bi * "
          "512, A), by := eq_sym(512, 256 * 2, tile_div_check_i115)");
      __ghost(rewrite_linear,
              "inside := fun (i: int) -> &d_partial_sums[MINDEX1(exact_div(N, "
              "512), bi)] ~~>[GMem] reduce_sum(i, A) -. reduce_sum(bi * 512, "
              "A), by := mul_add_factor(bi, 512)");
    }
    __ghost(assume, "P := (exact_div(N, 512) = MSIZE1(exact_div(N, 512)))");
    __ghost(
        rewrite_linear,
        "from := exact_div(N, 512), to := MSIZE1(exact_div(N, 512)), inside := "
        "fun (sz: int) -> for i in 0..sz -> for i1 in 0..256 -> "
        "&tile[MINDEX2(sz, 256, DMINDEX1(sz, i), i1)] ~> UninitCellOf(SMem)");
    __smem_free1(tile, 256);
    __ghost(give_smem_token, "tok_sz := sizeof(float) * 256");
    kernel_kill();
  } /*kernel_sequence@*/
  memcpy_device_to_host1(partial_sums, d_partial_sums, exact_div(N, 512));
  __ghost([&]() {
    __preserves("d_partial_sums ~> UninitMatrix1Of(exact_div(N, 512), GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_partial_sums);
  for (int bi = 0; bi < exact_div(N, 512); bi++) {
    __spreserves("&sum ~~> reduce_sum(bi * 512, A)");
    __xreads(
        "&partial_sums[MINDEX1(exact_div(N, 512), bi)] ~~> reduce_sum((bi + 1) "
        "* 512, A) -. reduce_sum(bi * 512, A)");
    sum = ({
      __rewrite_sequence;
      ({
        float get = ({
          __rewrite_sequence;
          ({
            float arith_res =
                partial_sums[MINDEX1(exact_div(N, 512), bi)] + sum;
            __ghost([&]() {
              __requires("res: float");
              __consumes("&arith_res ~~> res");
              __produces(
                  "&arith_res ~~> reduce_sum((bi + 1) * 512, A) -. "
                  "reduce_sum(bi * 512, A) +. reduce_sum(bi * 512, A)");
              __admitted();
            });
            const float arith_res9 = arith_res;
            arith_res9;
          });
        });
        __ghost(rewrite_float_linear,
                "inside := fun (v: float) -> &get ~~> v, by := "
                "r_cancel_minus_plus(reduce_sum((bi + 1) * 512, A), "
                "reduce_sum(bi * 512, A))");
        const float getc = get;
        getc;
      });
    });
  }
  free(partial_sums);
  __ghost([&]() {
    __preserves("d_arr ~> UninitMatrix1Of(N, GMem)");
    __admitted();
    __with("justif := shift_groups");
  });
  gmem_free(d_arr);
  __ghost(rewrite_linear,
          "inside := fun (i: int) -> &sum ~~> reduce_sum(i, A), by := "
          "eq_sym(N, exact_div(N, 512) * 512, tile_div_check_i)");
  return sum;
}
