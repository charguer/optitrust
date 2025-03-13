#include <optitrust.h>

__DECL(Triv, "int -> Prop");

__GHOST(trivial_init) {
  __requires("k: int");
  __ensures("Triv(k)");
  __admitted();
}

__GHOST(trivial_change) {
  __requires("k: int, old_k: int, Triv(old_k)");
  __ensures("Triv(k)");
  __admitted();
}

void req_triv(int k) {
  __requires("Triv(k)");
}

void test_clear(int k) {
  __pure();

  __ASSERT(refl_k1, "k = k");
  __clear("refl_k1");
  __ASSERT(refl_k2, "k = k");
  __ASSERT(refl_k3, "k = k");
}

void clear_in_for() {
  __pure();

  __ghost(trivial_init, "0", "triv_0");
  for (int i = 0; i < 20; i++) {
    req_triv(0);
    __clear("triv_0");
  }
  req_triv(0);
}

void clear_in_lambda() {
  __pure();

  __ghost(trivial_init, "0", "triv_0");
  const auto f = [&] {
    __pure();
    req_triv(0);
    __clear("triv_0");
  };
  req_triv(0);
}

void bind_clear(int k) {
  __pure();
  __ASSERT(_, "k = k");
  __ASSERT(refl_k, "k = k");
  __ASSERT(_, "k = k");
}
