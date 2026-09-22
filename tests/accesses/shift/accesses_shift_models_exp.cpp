#include <optitrust_models.h>

void test_var() {
  __pure();
  int x = ({
    __rewrite_sequence;
    ({
      int arith_res = 5;
      __ghost([&]() {
        __requires("res: int");
        __consumes("&arith_res ~~> res");
        __produces("&arith_res ~~> 0 + 5");
        __admitted();
      });
      const int arith_res2 = arith_res;
      arith_res2;
    });
  });
  x = ({
    __rewrite_sequence;
    ({
      int arith_res = 1 + 5;
      __ghost([&]() {
        __requires("res: int");
        __consumes("&arith_res ~~> res");
        __produces("&arith_res ~~> 1 + 5");
        __admitted();
      });
      const int arith_res3 = arith_res;
      arith_res3;
    });
  });
  int y = ({
            __rewrite_sequence;
            ({
              int get = ({
                __rewrite_sequence;
                ({
                  int arith_res = x - 5;
                  __ghost([&]() {
                    __requires("res: int");
                    __consumes("&arith_res ~~> res");
                    __produces("&arith_res ~~> 1 + 5 - 5");
                    __admitted();
                  });
                  const int arith_res4 = arith_res;
                  arith_res4;
                });
              });
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
      int arith_res = 2 * ({
                        __rewrite_sequence;
                        ({
                          int get = ({
                            __rewrite_sequence;
                            ({
                              int arith_res = x - 5;
                              __ghost([&]() {
                                __requires("res: int");
                                __consumes("&arith_res ~~> res");
                                __produces("&arith_res ~~> 1 + 5 - 5");
                                __admitted();
                              });
                              const int arith_res5 = arith_res;
                              arith_res5;
                            });
                          });
                          __ghost(rewrite_linear,
                                  "inside := fun (v: int) -> &get ~~> v, by := "
                                  "z_cancel_plus_minus(1, 5)");
                          const int getc = get;
                          getc;
                        });
                      }) +
                      5;
      __ghost([&]() {
        __requires("res: int");
        __consumes("&arith_res ~~> res");
        __produces("&arith_res ~~> 1 * 2 + 5");
        __admitted();
      });
      const int arith_res6 = arith_res;
      arith_res6;
    });
  });
}

void test_var_inv(int* t, int n) {
  __requires("T: int -> int");
  __reads("t ~> Matrix1(n, T)");
  int r = 0;
  {
    int s = ({
      __rewrite_sequence;
      ({
        int arith_res = 1;
        __ghost([&]() {
          __requires("res: int");
          __consumes("&arith_res ~~> res");
          __produces("&arith_res ~~> 0 + 1");
          __admitted();
        });
        const int arith_res8 = arith_res;
        arith_res8;
      });
    });
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i + 1");
      s++;
    }
    r = ({
      __rewrite_sequence;
      ({
        int get = ({
          __rewrite_sequence;
          ({
            int arith_res = s - 1;
            __ghost([&]() {
              __requires("res: int");
              __consumes("&arith_res ~~> res");
              __produces("&arith_res ~~> n + 1 - 1");
              __admitted();
            });
            const int arith_res9 = arith_res;
            arith_res9;
          });
        });
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
    int s = ({
      __rewrite_sequence;
      ({
        int arith_res = 1;
        __ghost([&]() {
          __requires("res: int");
          __consumes("&arith_res ~~> res");
          __produces("&arith_res ~~> 0 + 1");
          __admitted();
        });
        const int arith_res10 = arith_res;
        arith_res10;
      });
    });
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i + 1");
      s++;
      s = ({
        __rewrite_sequence;
        ({
          int arith_res = s - 1 - 1 + 1;
          __ghost([&]() {
            __requires("res: int");
            __consumes("&arith_res ~~> res");
            __produces("&arith_res ~~> i + 1 + 1 - 1 - 1 + 1");
            __admitted();
          });
          const int arith_res11 = arith_res;
          arith_res11;
        });
      });
      __ghost(rewrite_linear,
              "inside := fun (v: int) -> &s ~~> v - 1 + 1, by := "
              "z_cancel_plus_minus(i + 1, 1)");
      s = ({
        __rewrite_sequence;
        ({
          int arith_res = s - 1 + 1 + 1;
          __ghost([&]() {
            __requires("res: int");
            __consumes("&arith_res ~~> res");
            __produces("&arith_res ~~> i + 1 - 1 + 1 - 1 + 1 + 1");
            __admitted();
          });
          const int arith_res12 = arith_res;
          arith_res12;
        });
      });
      __ghost(rewrite_linear,
              "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
              "z_cancel_plus_minus(i + 1 - 1, 1)");
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + "
              "1, 1)");
      s = ({
        __rewrite_sequence;
        ({
          int arith_res = ({
                            __rewrite_sequence;
                            ({
                              int get = ({
                                __rewrite_sequence;
                                ({
                                  int arith_res = s - 1;
                                  __ghost([&]() {
                                    __requires("res: int");
                                    __consumes("&arith_res ~~> res");
                                    __produces("&arith_res ~~> i + 1 + 1 - 1");
                                    __admitted();
                                  });
                                  const int arith_res13 = arith_res;
                                  arith_res13;
                                });
                              });
                              __ghost(rewrite_linear,
                                      "inside := fun (v: int) -> &get ~~> v, "
                                      "by := z_cancel_plus_minus(i + 1, 1)");
                              const int getc = get;
                              getc;
                            });
                          }) -
                          1 + 1;
          __ghost([&]() {
            __requires("res: int");
            __consumes("&arith_res ~~> res");
            __produces("&arith_res ~~> i + 1 - 1 + 1");
            __admitted();
          });
          const int arith_res14 = arith_res;
          arith_res14;
        });
      });
      s = ({
        __rewrite_sequence;
        ({
          int arith_res =
              ({
                __rewrite_sequence;
                ({
                  int get = ({
                    __rewrite_sequence;
                    ({
                      int arith_res = s - 1;
                      __ghost([&]() {
                        __requires("res: int");
                        __consumes("&arith_res ~~> res");
                        __produces("&arith_res ~~> i + 1 - 1 + 1 - 1");
                        __admitted();
                      });
                      const int arith_res15 = arith_res;
                      arith_res15;
                    });
                  });
                  __ghost(rewrite_linear,
                          "inside := fun (v: int) -> &get ~~> v, by := "
                          "z_cancel_plus_minus(i + 1 - 1, 1)");
                  const int getc = get;
                  getc;
                });
              }) +
              1 + 1;
          __ghost([&]() {
            __requires("res: int");
            __consumes("&arith_res ~~> res");
            __produces("&arith_res ~~> i + 1 - 1 + 1 + 1");
            __admitted();
          });
          const int arith_res16 = arith_res;
          arith_res16;
        });
      });
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := z_cancel_minus_plus(i + "
              "1, 1)");
    }
    s = ({
      __rewrite_sequence;
      ({
        int arith_res = ({
                          __rewrite_sequence;
                          ({
                            int get = ({
                              __rewrite_sequence;
                              ({
                                int arith_res = s - 1;
                                __ghost([&]() {
                                  __requires("res: int");
                                  __consumes("&arith_res ~~> res");
                                  __produces("&arith_res ~~> n + 1 - 1");
                                  __admitted();
                                });
                                const int arith_res17 = arith_res;
                                arith_res17;
                              });
                            });
                            __ghost(rewrite_linear,
                                    "inside := fun (v: int) -> &get ~~> v, by "
                                    ":= z_cancel_plus_minus(n, 1)");
                            const int getc = get;
                            getc;
                          });
                        }) +
                        1 + 1;
        __ghost([&]() {
          __requires("res: int");
          __consumes("&arith_res ~~> res");
          __produces("&arith_res ~~> n + 1 + 1");
          __admitted();
        });
        const int arith_res18 = arith_res;
        arith_res18;
      });
    });
    s = ({
      __rewrite_sequence;
      ({
        int arith_res = s - 1 + 1 + 1;
        __ghost([&]() {
          __requires("res: int");
          __consumes("&arith_res ~~> res");
          __produces("&arith_res ~~> n + 1 + 1 - 1 + 1 + 1");
          __admitted();
        });
        const int arith_res19 = arith_res;
        arith_res19;
      });
    });
    __ghost(rewrite_linear,
            "inside := fun (v: int) -> &s ~~> v + 1 + 1, by := "
            "z_cancel_plus_minus(n + 1, 1)");
    s++;
    r = ({
      __rewrite_sequence;
      ({
        int get = ({
          __rewrite_sequence;
          ({
            int arith_res = s - 1;
            __ghost([&]() {
              __requires("res: int");
              __consumes("&arith_res ~~> res");
              __produces("&arith_res ~~> n + 1 + 1 + 1 + 1 - 1");
              __admitted();
            });
            const int arith_res20 = arith_res;
            arith_res20;
          });
        });
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
    int s = ({
      __rewrite_sequence;
      ({
        int arith_res = 1 + 1;
        __ghost([&]() {
          __requires("res: int");
          __consumes("&arith_res ~~> res");
          __produces("&arith_res ~~> 1 + 1");
          __admitted();
        });
        const int arith_res21 = arith_res;
        arith_res21;
      });
    });
    __ghost(rewrite_linear, "inside := fun v -> &s ~~> v + 1, by := p01");
    for (int i = 0; i < n; i++) {
      __spreserves("&s ~~> i + 1 + 1");
      s = ({
        __rewrite_sequence;
        ({
          int arith_res = ({
                            __rewrite_sequence;
                            ({
                              int get = ({
                                __rewrite_sequence;
                                ({
                                  int arith_res = s - 1;
                                  __ghost([&]() {
                                    __requires("res: int");
                                    __consumes("&arith_res ~~> res");
                                    __produces("&arith_res ~~> i + 1 + 1 - 1");
                                    __admitted();
                                  });
                                  const int arith_res22 = arith_res;
                                  arith_res22;
                                });
                              });
                              __ghost(rewrite_linear,
                                      "inside := fun (v: int) -> &get ~~> v, "
                                      "by := z_cancel_plus_minus(i + 1, 1)");
                              const int getc = get;
                              getc;
                            });
                          }) -
                          1 + 2 + 1;
          __ghost([&]() {
            __requires("res: int");
            __consumes("&arith_res ~~> res");
            __produces("&arith_res ~~> i + 1 - 1 + 2 + 1");
            __admitted();
          });
          const int arith_res23 = arith_res;
          arith_res23;
        });
      });
      __ghost(rewrite_linear,
              "inside := fun v -> &s ~~> v + 1, by := p1(i + 1)");
    }
  }
}
