#include <optitrust_models.h>

void rowSum(int w, int* s, int* d, int n, int cn) {
  if (w == 3) /*@w*/ {
    for (int ic = 0; ic < n * cn; ic++) {
      int sum = 0;
      for (int k = ic / cn; k < ic / cn + 3; k++) {
        sum += s[MINDEX2(n + 2, cn, k, ic % cn)];
      }
      d[MINDEX2(n, cn, ic / cn, ic % cn)] = sum;
    }
  } /*w@*/
  else {
    if (w == 5) /*@w*/ {
      for (int ic = 0; ic < n * cn; ic++) {
        int sum = 0;
        for (int k = ic / cn; k < ic / cn + 5; k++) {
          sum += s[MINDEX2(n + 4, cn, k, ic % cn)];
        }
        d[MINDEX2(n, cn, ic / cn, ic % cn)] = sum;
      }
    } /*w@*/
    else /*@anyw*/ {
      if (cn == 1) /*@cn*/ {
        // __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
        int sum = 0;
        for (int k = 0; k < 0 + w; k++) {
          sum += s[MINDEX2(n + w - 1, 1, k, 0)];
        }
        d[MINDEX2(n, 1, 0, 0)] = sum;
        /*
        __ghost(to_prove, "P := __is_true(0 == 0 + 1 - 1)",
                "H <- H");
        __ghost(to_prove,
                "P := __is_true(0 + w == 0 + 1 + w - 1)",
                "H2 <- H");
        */
        for (int i = 0 + 1; i < n; i += 1) {
          /*
          __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
          __ghost(to_prove, "P := in_range(0, 0..1)");
          */
          sum += s[MINDEX2(n + w - 1, 1, i + w - 1, 0)] -
                 s[MINDEX2(n + w - 1, 1, i - 1, 0)];
          /*
          __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
          __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                  "H3 <- H");
          __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                  "H4 <- H");
          */
          d[MINDEX2(n, 1, i, 0)] = sum;
          /*
          __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
          __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                  "H6 <- H");
          */
        }
      } /*cn@*/
      else {
        if (cn == 3) /*@cn*/ {
          // __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          int sum = 0;
          /*
          __ghost(to_prove,
                  "P := __is_true(0 == 0 + 1 - 1)",
                  "H <- H");
          __ghost(to_prove,
                  "P := __is_true(0 + w == 0 + 1 + w - 1)",
                  "H2 <- H");
          __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          */
          int sum7 = 0;
          /*
          __ghost(to_prove,
                  "P := __is_true(0 == 0 + 1 - 1)",
                  "H8 <- H");
          __ghost(to_prove,
                  "P := __is_true(0 + w == 0 + 1 + w - 1)",
                  "H29 <- H");
          __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
          */
          int sum10 = 0;
          for (int k = 0; k < 0 + w; k++) {
            sum += s[MINDEX2(n + w - 1, 3, k, 0)];
            sum7 += s[MINDEX2(n + w - 1, 3, k, 1)];
            sum10 += s[MINDEX2(n + w - 1, 3, k, 2)];
          }
          d[MINDEX2(n, 3, 0, 0)] = sum;
          d[MINDEX2(n, 3, 0, 1)] = sum7;
          d[MINDEX2(n, 3, 0, 2)] = sum10;
          /*
          __ghost(to_prove,
                  "P := __is_true(0 == 0 + 1 - 1)",
                  "H11 <- H");
          __ghost(to_prove,
                  "P := __is_true(0 + w == 0 + 1 + w - 1)",
                  "H212 <- H");
          */
          for (int i = 0 + 1; i < n; i += 1) {
            /*
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(0, 0..3)");
            const __ghost_fn __ghost_pair_1 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i - 1, j := 0");
            __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(0, 0..3)");
            */
            sum += s[MINDEX2(n + w - 1, 3, i + w - 1, 0)] -
                   s[MINDEX2(n + w - 1, 3, i - 1, 0)];
            /*
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H3 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H4 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H6 <- H");
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(1, 0..3)");
            __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(1, 0..3)");
            */
            sum7 += s[MINDEX2(n + w - 1, 3, i + w - 1, 1)] -
                    s[MINDEX2(n + w - 1, 3, i - 1, 1)];
            /*
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H31 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H332 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H433 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H534 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H635 <- H");
            __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(2, 0..3)");
            const __ghost_fn __ghost_pair_122 = __ghost_begin(
                ro_matrix2_focus, "matrix := s, i := i - 1, j := 2");
            __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
            __ghost(to_prove, "P := in_range(2, 0..3)");
            */
            sum10 += s[MINDEX2(n + w - 1, 3, i + w - 1, 2)] -
                     s[MINDEX2(n + w - 1, 3, i - 1, 2)];
            /*
            __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H24 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                    "H325 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                    "H426 <- H");
            */
            d[MINDEX2(n, 3, i, 0)] = sum;
            d[MINDEX2(n, 3, i, 1)] = sum7;
            d[MINDEX2(n, 3, i, 2)] = sum10;
            /*
            __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H527 <- H");
            __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                    "H628 <- H");
            */
          }
        } /*cn@*/
        else {
          if (cn == 4) /*@cn*/ {
            // __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            int sum = 0;
            /*
            __ghost(to_prove,
                    "P := __is_true(0 == 0 + 1 - 1)",
                    "H <- H");
            __ghost(
                to_prove,
                "P := __is_true(0 + w == 0 + 1 + w - 1)",
                "H2 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            */
            int sum13 = 0;
            /*
            __ghost(to_prove,
                    "P := __is_true(0 == 0 + 1 - 1)",
                    "H14 <- H");
            __ghost(
                to_prove,
                "P := __is_true(0 + w == 0 + 1 + w - 1)",
                "H215 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            */
            int sum16 = 0;
            /*
            __ghost(to_prove,
                    "P := __is_true(0 == 0 + 1 - 1)",
                    "H17 <- H");
            __ghost(
                to_prove,
                "P := __is_true(0 + w == 0 + 1 + w - 1)",
                "H218 <- H");
            __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
            */
            int sum19 = 0;
            for (int k = 0; k < 0 + w; k++) {
              sum += s[MINDEX2(n + w - 1, 4, k, 0)];
              sum13 += s[MINDEX2(n + w - 1, 4, k, 1)];
              sum16 += s[MINDEX2(n + w - 1, 4, k, 2)];
              sum19 += s[MINDEX2(n + w - 1, 4, k, 3)];
            }
            d[MINDEX2(n, 4, 0, 0)] = sum;
            d[MINDEX2(n, 4, 0, 1)] = sum13;
            d[MINDEX2(n, 4, 0, 2)] = sum16;
            d[MINDEX2(n, 4, 0, 3)] = sum19;
            /*
            __ghost(to_prove,
                    "P := __is_true(0 == 0 + 1 - 1)",
                    "H20 <- H");
            __ghost(
                to_prove,
                "P := __is_true(0 + w == 0 + 1 + w - 1)",
                "H221 <- H");
            */
            for (int i = 0 + 1; i < n; i += 1) {
              /*
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(0, 0..4)");
              const __ghost_fn __ghost_pair_1 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 0");
              __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(0, 0..4)");
              */
              sum += s[MINDEX2(n + w - 1, 4, i + w - 1, 0)] -
                     s[MINDEX2(n + w - 1, 4, i - 1, 0)];
              /*
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)", "H <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H3 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H4 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H6 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(1, 0..4)");
              __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(1, 0..4)");
              */
              sum13 += s[MINDEX2(n + w - 1, 4, i + w - 1, 1)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 1)];
              /*
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H58 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H359 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H460 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H561 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H662 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(2, 0..4)");
              __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(2, 0..4)");
              */
              sum16 += s[MINDEX2(n + w - 1, 4, i + w - 1, 2)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 2)];
              /*
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H51 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H352 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H453 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H554 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H655 <- H");
              __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(3, 0..4)");
              const __ghost_fn __ghost_pair_142 = __ghost_begin(
                  ro_matrix2_focus, "matrix := s, i := i - 1, j := 3");
              __ghost(to_prove, "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
              __ghost(to_prove, "P := in_range(3, 0..4)");
              */
              sum19 += s[MINDEX2(n + w - 1, 4, i + w - 1, 3)] -
                       s[MINDEX2(n + w - 1, 4, i - 1, 3)];
              /*
              __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                      "H44 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                      "H345 <- H");
              __ghost(to_prove,
                      "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                      "H446 <- H");
              __ghost(rewrite_linear,
                      "inside := [&] (int v) -> HProp  &sum19 ~~> v, by := "
                      "reduce_int_sum_slide(i - 1, i + w - 1, i + 1 - 1, i + 1 "
                      "+ w - 1, fun k0 -> S(k0, 3), H44, H345, H446)");
              */
              d[MINDEX2(n, 4, i, 0)] = sum;
              d[MINDEX2(n, 4, i, 1)] = sum13;
              d[MINDEX2(n, 4, i, 2)] = sum16;
              d[MINDEX2(n, 4, i, 3)] = sum19;
              /*
              __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H547 <- H");
              __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                      "H648 <- H");
              */
            }
          } /*cn@*/
          else /*@anycn*/ {
            for (int c = 0; c < cn; c++) {
              // __ghost(to_prove, "P := is_subrange(0..(0 + 1), 0..n)");
              int sum = 0;
              for (int k = 0; k < 0 + w; k++) {
                sum += s[MINDEX2(n + w - 1, cn, k, c)];
              }
              d[MINDEX2(n, cn, 0, c)] = sum;
              /*
              __ghost(to_prove,
                      "P := __is_true(0 == 0 + 1 - 1)",
                      "H <- H");
              __ghost(
                  to_prove,
                  "P := __is_true(0 + w == 0 + 1 + w - 1)",
                  "H2 <- H");
              */
              for (int i = 0 + 1; i < n; i += 1) {
                /*
                __ghost(to_prove, "P := in_range(i - 1, 0..(n + w - 1))");
                __ghost(to_prove, "P := in_range(c, 0..cn)");
                const __ghost_fn __ghost_pair_1 = __ghost_begin(
                    ro_matrix2_focus, "matrix := s, i := i - 1, j := c");
                __ghost(to_prove,
                        "P := in_range(i + 1 + w - 1, 0..(n + w - 1))");
                __ghost(to_prove, "P := in_range(c, 0..cn)");
                */
                sum += s[MINDEX2(n + w - 1, cn, i + w - 1, c)] -
                       s[MINDEX2(n + w - 1, cn, i - 1, c)];
                /*
                __ghost(to_prove, "P := __is_true(i + w - 1 >= i - 1)",
                        "H <- H");
                __ghost(to_prove, "P := __is_true(i + 1 - 1 == i - 1 + 1)",
                        "H3 <- H");
                __ghost(to_prove,
                        "P := __is_true(i + 1 + w - 1 == i + w - 1 + 1)",
                */
                d[MINDEX2(n, cn, i, c)] = sum;
                /*
                __ghost(to_prove, "P := __is_true(i + 1 - 1 == i)", "H5 <- H");
                __ghost(to_prove, "P := __is_true(i + 1 + w - 1 == i + w)",
                        "H6 <- H");
                */
              }
            }
          } /*anycn@*/
        }
      }
    } /*anyw@*/
  }
}
