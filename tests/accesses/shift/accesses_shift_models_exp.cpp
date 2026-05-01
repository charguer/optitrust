#include <optitrust_models.h>

void test_var() {
  __pure();
  int x = 0 + 5;
  x = 1 + 5;
  int y = ({
            __rewrite_sequence;
            ({
              int get = x - 5;
              __ghost(rewrite_linear,
                      "inside := fun (v: int) -> &get ~~> v, by := "
                      "z_cancel_plus_minus(1, 5)");
              const int getc = get;
              getc;
            });
          }) *
          1;
  x = ({
        __rewrite_sequence;
        ({
          int get = x - 5;
          __ghost(rewrite_linear,
                  "inside := fun (v: int) -> &get ~~> v, by := "
                  "z_cancel_plus_minus(1, 5)");
          const int getc = get;
          getc;
        });
      }) * 2 +
      5;
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");
  int r = 0;
  {
    int s = 0 + 1;
    for (int i = 0; i < n; i++) {
      __strict();
      __spreserves("&s ~~> i + 1");
      s++;
    }
    r = ({
      __rewrite_sequence;
      ({
        int get = s - 1;
        __ghost(rewrite_linear,
                "inside := fun (v: int) -> &get ~~> v, by := "
                "z_cancel_plus_minus(n, 1)");
        const int getc = get;
        getc;
      });
    });
    __ghost(assert_hprop, "H := &r ~~> n");
  }
  {
    int s = 0 + 1;
    for (int i = 0; i < n; i++) {
      __strict();
      __spreserves("&s ~~> i + 1");
      s++;
      s = s - 1 - 1 + 1;
      __ghost(rewrite_linear,
              "inside := fun (v: int) -> &s ~~> v - 1 + 1, by := "
              "z_cancel_plus_minus(i + 1, 1)");
      s = s - 1 + 1 + 1;
      __ghost(rewrite_linear,
              "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
              "z_cancel_plus_minus(i + 1 - 1, 1)");
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + "
              "1, 1)");
      s = ({
            __rewrite_sequence;
            ({
              int get = s - 1;
              __ghost(rewrite_linear,
                      "inside := fun (v: int) -> &get ~~> v, by := "
                      "z_cancel_plus_minus(i + 1, 1)");
              const int getc = get;
              getc;
            });
          }) -
          1 + 1;
      s = ({
            __rewrite_sequence;
            ({
              int get = s - 1;
              __ghost(rewrite_linear,
                      "inside := fun (v: int) -> &get ~~> v, by := "
                      "z_cancel_plus_minus(i + 1 - 1, 1)");
              const int getc = get;
              getc;
            });
          }) +
          1 + 1;
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + "
              "1, 1)");
    }
    s = ({
          __rewrite_sequence;
          ({
            int get = s - 1;
            __ghost(rewrite_linear,
                    "inside := fun (v: int) -> &get ~~> v, by := "
                    "z_cancel_plus_minus(n, 1)");
            const int getc = get;
            getc;
          });
        }) +
        1 + 1;
    s = s - 1 + 1 + 1;
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
            "z_cancel_plus_minus(n + 1, 1)");
    s++;
    r = ({
      __rewrite_sequence;
      ({
        int get = s - 1;
        __ghost(rewrite_linear,
                "inside := fun (v: int) -> &get ~~> v, by := "
                "z_cancel_plus_minus(n + 1 + 1 + 1, 1)");
        const int getc = get;
        getc;
      });
    });
    __ghost(assert_hprop, "H := &r ~~> n + 1 + 1 + 1");
  }
  {
    __ghost(assume, "P := forall (n: int) -> (n - 1 + 2 = n + 1)", "p1 <- H");
    __ghost(assume, "P := (1 = 0 + 1)", "p01 <- H");
    int s = 1 + 1;
    __ghost(rewrite_linear, "inside := fun v -> &s ~~> v + 1, by := p01");
    for (int i = 0; i < n; i++) {
      __strict();
      __spreserves("&s ~~> i + 1 + 1");
      s = ({
            __rewrite_sequence;
            ({
              int get = s - 1;
              __ghost(rewrite_linear,
                      "inside := fun (v: int) -> &get ~~> v, by := "
                      "z_cancel_plus_minus(i + 1, 1)");
              const int getc = get;
              getc;
            });
          }) -
          1 + 2 + 1;
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := p1(i + 1)");
    }
  }
}
